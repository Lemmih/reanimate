{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
module Reanimate.Scene.Object where

import           Control.Lens
import           Control.Monad          (forM_, void)
import           Control.Monad.State    (State, execState)
import           Graphics.SvgTree       (Number (..), Tree, pattern None, strokeWidth, toUserUnit)
import           Linear.V2              (R1 (_x), R2 (_y), V2 (..))
import           Linear.Vector          (Additive (lerp), (^*))
import           Reanimate.Animation
import           Reanimate.Constants    (defaultDPI, defaultStrokeWidth)
import           Reanimate.Ease         (Signal, curveS, fromToS)
import           Reanimate.Effect       (applyE, fadeLineOutE, overEnding)
import           Reanimate.Math.Balloon (balloon)
import           Reanimate.Morph.Common (morph)
import           Reanimate.Morph.Linear (linear)
import           Reanimate.Svg

import           Reanimate.Scene.Core   (Scene, ZIndex, fork, scene, wait)
import           Reanimate.Scene.Sprite (Frame, Sprite, destroySprite, newSprite, newSpriteA', play,
                                         spriteDuration, spriteModify, spriteT, unVar)
import           Reanimate.Scene.Var    (Var, modifyVar, newVar, readVar, tweenVar)

-------------------------------------------------------
-- Objects

-- | Objects can be any Haskell structure as long as it can be rendered to SVG.
class Renderable a where
  toSVG :: a -> SVG

instance Renderable Tree where
  toSVG = id

-- | Objects are SVG nodes (represented as Haskell values) with
--   identity, location, and several other properties that can
--   change over time.
data Object s a = Object
  { objectSprite :: Sprite s,
    objectData   :: Var s (ObjectData a)
  }

-- | Container for object properties.
data ObjectData a = ObjectData
  { _oTranslate   :: V2 Double,
    _oValueRef    :: a,
    _oSVG         :: SVG,
    _oContext     :: SVG -> SVG,
    -- | Top, right, bottom, left
    _oMargin      :: (Double, Double, Double, Double),
    _oBB          :: (Double, Double, Double, Double),
    _oOpacity     :: Double,
    _oShown       :: Bool,
    _oZIndex      :: ZIndex,
    _oEasing      :: Signal,
    _oScale       :: Double,
    _oScaleOrigin :: V2 Double
  }

-- Basic lenses

-- FIXME: Maybe 'position' is a better name.

-- | Object position. Default: \<0,0\>
oTranslate :: Lens' (ObjectData a) (V2 Double)
oTranslate = lens _oTranslate $ \obj val -> obj {_oTranslate = val}

-- | Object X position. Default: 0
oTranslateX :: Lens' (ObjectData a) Double
oTranslateX = oTranslate . _x

-- | Object Y position. Default: 0
oTranslateY :: Lens' (ObjectData a) Double
oTranslateY = oTranslate . _y

-- | Rendered SVG node of an object. Does not include context
--   or object properties. Read-only.
oSVG :: Getter (ObjectData a) SVG
oSVG = to _oSVG

-- | Custom render context. Is applied to the object for every
--   frame that it is shown.
oContext :: Lens' (ObjectData a) (SVG -> SVG)
oContext = lens _oContext $ \obj val -> obj {_oContext = val}

-- | Object margins (top, right, bottom, left) in local units.
oMargin :: Lens' (ObjectData a) (Double, Double, Double, Double)
oMargin = lens _oMargin $ \obj val -> obj {_oMargin = val}

-- | Object bounding-box (minimal X-coordinate, minimal Y-coordinate,
--   width, height). Uses `Reanimate.Svg.BoundingBox.boundingBox`
--   and has the same limitations.
oBB :: Getter (ObjectData a) (Double, Double, Double, Double)
oBB = to _oBB

-- | Object opacity. Default: 1
oOpacity :: Lens' (ObjectData a) Double
oOpacity = lens _oOpacity $ \obj val -> obj {_oOpacity = val}

-- | Toggle for whether or not the object should be rendered.
--   Default: False
oShown :: Lens' (ObjectData a) Bool
oShown = lens _oShown $ \obj val -> obj {_oShown = val}

-- | Object's z-index.
oZIndex :: Lens' (ObjectData a) ZIndex
oZIndex = lens _oZIndex $ \obj val -> obj {_oZIndex = val}

-- | Easing function used when modifying object properties.
--   Default: @'Reanimate.Ease.curveS' 2@
oEasing :: Lens' (ObjectData a) Signal
oEasing = lens _oEasing $ \obj val -> obj {_oEasing = val}

-- | Object's scale. Default: 1
oScale :: Lens' (ObjectData a) Double
oScale = lens _oScale $ \obj val -> oComputeBB obj {_oScale = val}

-- | Origin point for scaling. Default: \<0,0\>
oScaleOrigin :: Lens' (ObjectData a) (V2 Double)
oScaleOrigin = lens _oScaleOrigin $ \obj val -> oComputeBB obj {_oScaleOrigin = val}

-- Smart lenses

-- | Lens for the source value contained in an object.
oValue :: Renderable a => Lens' (ObjectData a) a
oValue = lens _oValueRef $ \obj newVal ->
  let svg = toSVG newVal
   in oComputeBB
        obj
          { _oValueRef = newVal,
            _oSVG = svg
          }

oComputeBB :: ObjectData a -> ObjectData a
oComputeBB obj =
  obj
    { _oBB = boundingBox $ oScaleApply obj (_oSVG obj)
    }

-- | Derived location of the top-most point of an object + margin.
oTopY :: Lens' (ObjectData a) Double
oTopY = lens getter setter
  where
    getter obj =
      let top = obj ^. oMarginTop
          miny = obj ^. oBBMinY
          h = obj ^. oBBHeight
          dy = obj ^. oTranslate . _2
       in dy + miny + h + top
    setter obj val =
      obj & (oTranslate . _2) +~ val - getter obj

-- | Derived location of the bottom-most point of an object + margin.
oBottomY :: Lens' (ObjectData a) Double
oBottomY = lens getter setter
  where
    getter obj =
      let bot = obj ^. oMarginBottom
          miny = obj ^. oBBMinY
          dy = obj ^. oTranslate . _2
       in dy + miny - bot
    setter obj val =
      obj & (oTranslate . _2) +~ val - getter obj

-- | Derived location of the left-most point of an object + margin.
oLeftX :: Lens' (ObjectData a) Double
oLeftX = lens getter setter
  where
    getter obj =
      let left = obj ^. oMarginLeft
          minx = obj ^. oBBMinX
          dx = obj ^. oTranslate . _1
       in dx + minx - left
    setter obj val =
      obj & (oTranslate . _1) +~ val - getter obj

-- | Derived location of the right-most point of an object + margin.
oRightX :: Lens' (ObjectData a) Double
oRightX = lens getter setter
  where
    getter obj =
      let right = obj ^. oMarginRight
          minx = obj ^. oBBMinX
          w = obj ^. oBBWidth
          dx = obj ^. oTranslate . _1
       in dx + minx + w + right
    setter obj val =
      obj & (oTranslate . _1) +~ val - getter obj

-- | Derived location of an object's center point.
oCenterXY :: Lens' (ObjectData a) (V2 Double)
oCenterXY = lens getter setter
  where
    getter obj =
      let minx = obj ^. oBBMinX
          miny = obj ^. oBBMinY
          w = obj ^. oBBWidth
          h = obj ^. oBBHeight
          V2 dx dy = obj ^. oTranslate
       in V2 (dx + minx + w / 2) (dy + miny + h / 2)
    setter obj (V2 dx dy) =
      let V2 x y = getter obj
       in obj & (oTranslate . _1) +~ dx - x
            & (oTranslate . _2) +~ dy - y

-- | Derived location of an object's center X value.
oCenterX :: Lens' (ObjectData a) Double
oCenterX = oCenterXY . _x

-- | Derived location of an object's center Y value.
oCenterY :: Lens' (ObjectData a) Double
oCenterY = oCenterXY . _y

-- | Object's top margin.
oMarginTop :: Lens' (ObjectData a) Double
oMarginTop = oMargin . _1

-- | Object's right margin.
oMarginRight :: Lens' (ObjectData a) Double
oMarginRight = oMargin . _2

-- | Object's bottom margin.
oMarginBottom :: Lens' (ObjectData a) Double
oMarginBottom = oMargin . _3

-- | Object's left margin.
oMarginLeft :: Lens' (ObjectData a) Double
oMarginLeft = oMargin . _4

-- | Object's minimal X-coordinate..
oBBMinX :: Getter (ObjectData a) Double
oBBMinX = oBB . _1

-- | Object's minimal Y-coordinate..
oBBMinY :: Getter (ObjectData a) Double
oBBMinY = oBB . _2

-- | Object's width without margin.
oBBWidth :: Getter (ObjectData a) Double
oBBWidth = oBB . _3

-- | Object's height without margin.
oBBHeight :: Getter (ObjectData a) Double
oBBHeight = oBB . _4

-------------------------------------------------------------------------------
-- Object modifiers

-- | Modify object properties.
oModify :: Object s a -> (ObjectData a -> ObjectData a) -> Scene s ()
oModify o = modifyVar (objectData o)

-- | Modify object properties using a stateful API.
oModifyS :: Object s a -> State (ObjectData a) b -> Scene s ()
oModifyS o = oModify o . execState

-- | Query object property.
oRead :: Object s a -> Getting b (ObjectData a) b -> Scene s b
oRead o l = view l <$> readVar (objectData o)

-- | Modify object properties over a set duration.
oTween :: Object s a -> Duration -> (Double -> ObjectData a -> ObjectData a) -> Scene s ()
oTween o d fn = do
  -- Read 'easing' var here instead of taking it from 'v'.
  -- This allows different easing functions even at the same timestamp.
  ease <- oRead o oEasing
  tweenVar (objectData o) d (\v t -> fn (ease t) v)

-- | Modify object properties over a set duration using a stateful API.
oTweenS :: Object s a -> Duration -> (Double -> State (ObjectData a) b) -> Scene s ()
oTweenS o d fn = oTween o d (execState . fn)

-- | Modify object value over a set duration. This is a convenience function
--   for modifying `oValue`.
oTweenV :: Renderable a => Object s a -> Duration -> (Double -> a -> a) -> Scene s ()
oTweenV o d fn = oTween o d (\t -> oValue %~ fn t)

-- | Modify object value over a set duration using a stateful API. This is a
--   convenience function for modifying `oValue`.
oTweenVS :: Renderable a => Object s a -> Duration -> (Double -> State a b) -> Scene s ()
oTweenVS o d fn = oTween o d (\t -> oValue %~ execState (fn t))

-- | Create new object.
oNew :: Renderable a => a -> Scene s (Object s a)
oNew = newObject

-- | Create new object.
newObject :: Renderable a => a -> Scene s (Object s a)
newObject val = do
  ref <-
    newVar
      ObjectData
        { _oTranslate = V2 0 0,
          _oValueRef = val,
          _oSVG = svg,
          _oContext = id,
          _oMargin = (0.5, 0.5, 0.5, 0.5),
          _oBB = boundingBox svg,
          _oOpacity = 1,
          _oShown = False,
          _oZIndex = 1,
          _oEasing = curveS 2,
          _oScale = 1,
          _oScaleOrigin = V2 0 0
        }
  sprite <- newSprite $ do
    ~obj@ObjectData {..} <- unVar ref
    pure $
      if _oShown
        then
          uncurryV2 translate _oTranslate $
            oScaleApply obj $
              withGroupOpacity _oOpacity $
                mkGroup [_oContext _oSVG]
        else None
  spriteModify sprite $ do
    ~ObjectData {_oZIndex = z} <- unVar ref
    pure $ \(img, _) -> (img, z)
  return
    Object
      { objectSprite = sprite,
        objectData = ref
      }
  where
    svg = toSVG val

oScaleApply :: ObjectData a -> (SVG -> SVG)
oScaleApply ObjectData {..} =
  uncurryV2 translate (negate _oScaleOrigin)
    . scale _oScale
    . uncurryV2 translate _oScaleOrigin

uncurryV2 :: (a -> a -> b) -> V2 a -> b
uncurryV2 fn (V2 a b) = fn a b

-------------------------------------------------------------------------------
-- Object-local sprites

oFrame :: Object s a -> Frame s (SVG -> SVG)
oFrame o = do
  ~obj@ObjectData {..} <- unVar (objectData o)
  pure $ \svg -> 
    if _oShown
      then uncurryV2 translate _oTranslate $
        oScaleApply obj $ withGroupOpacity _oOpacity $
          mkGroup [ _oContext svg ]
      else None

oNewSprite :: Object s a -> Frame s SVG -> Scene s (Sprite s)
oNewSprite o gen = do
  sprite <- newSprite $ oFrame o <*> gen
  spriteModify sprite $ do
    ~ObjectData {_oZIndex = z} <- unVar (objectData o)
    pure $ \(img, _) -> (img, z)
  return sprite

oNewSpriteA :: Object s a -> Animation -> Scene s (Sprite s)
oNewSpriteA = oNewSpriteA' SyncStretch

oNewSpriteA' :: Sync -> Object s a -> Animation -> Scene s (Sprite s)
oNewSpriteA' sync o animation =
  oNewSprite o (getAnimationFrame sync animation <$> spriteT <*> spriteDuration)
    <* wait (duration animation)

--
oPlay :: Object s a -> Animation -> Scene s ()
oPlay o ani = oNewSpriteA o ani >>= destroySprite

-------------------------------------------------------------------------------
-- Graphical transformations

-- | Instantly show object.
oShow :: Object s a -> Scene s ()
oShow o = oModify o $ oShown .~ True

-- | Instantly hide object.
oHide :: Object s a -> Scene s ()
oHide o = oModify o $ oShown .~ False

-- | Show object with an animator function. The animator is responsible for
--   transitioning the object from invisible to having its final shape.
--   If this doesn't hold true for the animator function then the final
--   animation will be discontinuous.
oShowWith :: Object s a -> (SVG -> Animation) -> Scene s ()
oShowWith o fn = do
  oModify o $ oShown .~ True
  initSVG <- oRead o oSVG
  let ani = fn initSVG
  oTween o (duration ani) $ \t obj ->
    obj {_oSVG = getAnimationFrame SyncStretch ani t 1}
  oModify o $ \obj -> obj {_oSVG = initSVG}

-- | Hide object with an animator function. The animator is responsible for
--   transitioning the object from visible to invisible.
--   If this doesn't hold true for the animator function then the final
--   animation will be discontinuous.
oHideWith :: Object s a -> (SVG -> Animation) -> Scene s ()
oHideWith o fn = do
  initSVG <- oRead o oSVG
  let ani = fn initSVG
  oTween o (duration ani) $ \t obj ->
    obj {_oSVG = getAnimationFrame SyncStretch ani t 1}
  oModify o $ \obj -> obj {_oSVG = initSVG}
  oModify o $ oShown .~ False

-- | Fade in object over a set duration.
oFadeIn :: SVG -> Animation
oFadeIn svg = animate $ \t -> withGroupOpacity t svg

-- | Fade out object over a set duration.
oFadeOut :: SVG -> Animation
oFadeOut = reverseA . oFadeIn

-- | Scale in object over a set duration.
--
--   Example:
--
-- @
-- do txt <- 'oNew' $ 'withStrokeWidth' 0 $ 'withFillOpacity' 1 $
--      'center' $ 'scale' 3 $ 'Reanimate.LaTeX.latex' "oGrow"
--    'oShowWith' txt 'oGrow'
--    'wait' 1; 'oHideWith' txt 'oFadeOut'
-- @
--
--    <<docs/gifs/doc_oGrow.gif>>
oGrow :: SVG -> Animation
oGrow svg = animate $ \t -> scale t svg

-- | Scale out object over a set duration.
oShrink :: SVG -> Animation
oShrink = reverseA . oGrow

-- | Relative coordinates for an SVG node.
type Origin = (Double, Double)

svgOrigin :: SVG -> Origin -> (Double, Double)
svgOrigin svg (originX, originY) =
  case boundingBox svg of
    (polyX, polyY, polyWidth, polyHeight) ->
      ( polyX + polyWidth * originX,
        polyY + polyHeight * originY
      )

-- | Scale in children from left to right, with an origin at the top of each child.
--
--   Example:
--
-- @
-- do txt <- 'oNew' $ 'withStrokeWidth' 0 $ 'withFillOpacity' 1 $
--      'center' $ 'scale' 3 $ 'Reanimate.LaTeX.latex' "oScaleIn"
--    'oShowWith' txt $ 'adjustDuration' (*2) . 'oScaleIn'
--    'wait' 1; 'oHideWith' txt 'oFadeOut'
-- @
--
--   <<docs/gifs/doc_oScaleIn.gif>>
oScaleIn :: SVG -> Animation
oScaleIn = oScaleIn' (curveS 2) (0.5, 1)

-- | Like 'oScaleIn' but takes an easing function and an origin.
oScaleIn' :: Signal -> Origin -> SVG -> Animation
oScaleIn' easing origin = oStagger' 0.05 $ \svg ->
  let (cx, cy) = svgOrigin svg origin
   in signalA easing $
        mkAnimation 0.3 $ \t ->
          translate cx cy $
            scale t $
              translate
                (- cx)
                (- cy)
                svg

-- | Scale out children from left to right, with an origin at the bottom of each child.
--
--   Example:
--
-- @
-- do txt <- 'oNew' $ 'withStrokeWidth' 0 $ 'withFillOpacity' 1 $
--      'center' $ 'scale' 3 $ 'Reanimate.LaTeX.latex' "oScaleOut"
--    'oShowWith' txt 'oFadeIn'
--    'oHideWith' txt $ 'adjustDuration' (*2) . 'oScaleOut'
-- @
--
--   <<docs/gifs/doc_oScaleOut.gif>>
oScaleOut :: SVG -> Animation
oScaleOut = reverseA . oStaggerRev' 0.05 (oScaleIn' (curveS 2) (0.5, 0))

-- | Like 'oScaleOut' but takes an easing function and an origin.
oScaleOut' :: Signal -> Origin -> SVG -> Animation
oScaleOut' easing origin = reverseA . oStaggerRev' 0.05 (oScaleIn' easing origin)

-- | Animate each child node in parallel.
oSim :: (SVG -> Animation) -> SVG -> Animation
oSim = oStagger' 0

-- oSim (oStagger fn) = oSim fn
-- oStagger (oStagger fn) = oStagger fn
-- | Animate each child node in parallel, staggered by 0.2 seconds.
oStagger :: (SVG -> Animation) -> SVG -> Animation
oStagger = oStagger' 0.2

-- | Animate each child node in parallel, staggered by 0.2 seconds and in reverse order.
oStaggerRev :: (SVG -> Animation) -> SVG -> Animation
oStaggerRev = oStaggerRev' 0.2

-- | Animate each child node in parallel, staggered by a given duration.
oStagger' :: Duration -> (SVG -> Animation) -> SVG -> Animation
oStagger' staggerDelay fn svg = scene $
  forM_ (svgGlyphs svg) $ \(ctx, _attr, node) -> do
    void $ fork $ newSpriteA' SyncFreeze (fn $ ctx node)
    wait staggerDelay

-- | Animate each child node in parallel, staggered by given duration and in reverse order.
oStaggerRev' :: Duration -> (SVG -> Animation) -> SVG -> Animation
oStaggerRev' staggerDelay fn svg = scene $
  forM_ (reverse $ svgGlyphs svg) $ \(ctx, _attr, node) -> do
    void $ fork $ newSpriteA' SyncFreeze (fn $ ctx node)
    wait staggerDelay

-- | Render SVG by first drawing outlines and then filling shapes.
--
--   Example:
--
-- @
-- do txt <- 'oNew' $ 'withStrokeWidth' 0 $ 'withFillOpacity' 1 $
--      'center' $ 'scale' 4 $ 'Reanimate.LaTeX.latex' "oDraw"
--    'oModify' txt $ 'oEasing' .~ id
--    'oShowWith' txt 'oDraw'; 'wait' 1
--    'oHideWith' txt 'oFadeOut'
-- @
--
--   <<docs/gifs/doc_oDraw.gif>>
oDraw :: SVG -> Animation
oDraw = oStagger $ \svg -> scene $
  forM_ (svgGlyphs $ pathify svg) $ \(ctx, attr, node) -> do
    let sWidth =
          case toUserUnit defaultDPI <$> (attr ^. strokeWidth) of
            Just (Num d) -> max defaultStrokeWidth d
            _            -> defaultStrokeWidth
    -- wait 1
    play $
      mapA ctx $
        applyE (overEnding fillDur $ fadeLineOutE sWidth) $
          animate $ \t ->
            withStrokeWidth sWidth $
              mkGroup
                [withFillOpacity 0 $ partialSvg t node]
    wait (- fillDur)
    newSpriteA' SyncFreeze $
      mkAnimation fillDur $ \t ->
        withGroupOpacity t $
          mkGroup [ctx node]
  where
    fillDur = 0.3

_oBalloon :: SVG -> Animation
_oBalloon = animate . balloon

-- FIXME: Also transform attributes: 'opacity', 'scale', 'scaleOrigin'.

-- | Morph source object into target object over a set duration.
oTransform :: Object s a -> Object s b -> Duration -> Scene s ()
oTransform src dst d = do
  srcSvg <- oRead src oSVG
  srcCtx <- oRead src oContext
  srcEase <- oRead src oEasing
  srcLoc <- oRead src oTranslate
  oModify src $ oShown .~ False

  dstSvg <- oRead dst oSVG
  dstCtx <- oRead dst oContext
  dstLoc <- oRead dst oTranslate

  m <- newObject $ Morph 0 (srcCtx srcSvg) (dstCtx dstSvg)
  oModifyS m $ do
    oShown .= True
    oEasing .= srcEase
    oTranslate .= srcLoc
  fork $ oTween m d $ \t -> oTranslate %~ lerp t dstLoc
  oTweenV m d $ \t -> morphDelta .~ t
  oModify m $ oShown .~ False
  oModify dst $ oShown .~ True

-------------------------------------------------------------------------------
-- Built-in objects

-- | Basic object mapping to \<circle\/\> in SVG.
newtype Circle = Circle {_circleRadius :: Double}

-- | Circle radius in local units.
circleRadius :: Lens' Circle Double
circleRadius = iso _circleRadius Circle

instance Renderable Circle where
  toSVG (Circle r) = mkCircle r

-- | Basic object mapping to \<rect\/\> in SVG.
data Rectangle = Rectangle {_rectWidth :: Double, _rectHeight :: Double}

-- | Rectangle width in local units.
rectWidth :: Lens' Rectangle Double
rectWidth = lens _rectWidth $ \obj val -> obj {_rectWidth = val}

-- | Rectangle height in local units.
rectHeight :: Lens' Rectangle Double
rectHeight = lens _rectHeight $ \obj val -> obj {_rectHeight = val}

instance Renderable Rectangle where
  toSVG (Rectangle w h) = mkRect w h

-- | Object representing an interpolation between SVG nodes.
data Morph = Morph {_morphDelta :: Double, _morphSrc :: SVG, _morphDst :: SVG}

-- | Control variable for the interpolation. A value of 0 gives the
--   source SVG and 1 gives the target svg.
morphDelta :: Lens' Morph Double
morphDelta = lens _morphDelta $ \obj val -> obj {_morphDelta = val}

-- | Source shape.
morphSrc :: Lens' Morph SVG
morphSrc = lens _morphSrc $ \obj val -> obj {_morphSrc = val}

-- | Target shape.
morphDst :: Lens' Morph SVG
morphDst = lens _morphDst $ \obj val -> obj {_morphDst = val}

instance Renderable Morph where
  toSVG (Morph t src dst) = morph linear src dst t

-- | Cameras can take control of objects and manipulate them
--   with convenient pan and zoom operations.
data Camera = Camera

instance Renderable Camera where
  toSVG Camera = None

-- | Connect an object to a camera such that
--   camera settings (position, zoom, and rotation) is
--   applied to the object.
--
--   Example
--
-- @
-- do cam \<- 'newObject' 'Camera'
--    circ \<- 'newObject' $ 'Circle' 2
--    'oModifyS' circ $
--      'oContext' .= 'withFillOpacity' 1 . 'withFillColor' "blue"
--    'oShow' circ
--    'cameraAttach' cam circ
--    'cameraZoom' cam 1 2
--    'cameraZoom' cam 1 1
-- @
--
--   <<docs/gifs/doc_cameraAttach.gif>>
cameraAttach :: Object s Camera -> Object s a -> Scene s ()
cameraAttach cam obj =
  spriteModify (objectSprite obj) $ do
    camData <- unVar (objectData cam)
    return $ \(svg, zindex) ->
      let V2 x y = camData ^. oTranslate
          ctx =
            translate (- x) (- y)
              . uncurryV2 translate (camData ^. oScaleOrigin)
              . scale (camData ^. oScale)
              . uncurryV2 translate (negate $ camData ^. oScaleOrigin)
       in (ctx svg, zindex)

-- |
--
--   Example
--
-- @
-- do cam \<- 'newObject' 'Camera'
--    circ \<- 'newObject' $ 'Circle' 2; 'oShow' circ
--    'oModify' circ $ 'oTranslate' .~ (-3,0)
--    box \<- 'newObject' $ 'Rectangle' 4 4; 'oShow' box
--    'oModify' box $ 'oTranslate' .~ (3,0)
--    'cameraAttach' cam circ
--    'cameraAttach' cam box
--    'cameraFocus' cam (-3,0)
--    'cameraZoom' cam 2 2      -- Zoom in
--    'cameraZoom' cam 2 1      -- Zoom out
--    'cameraFocus' cam (3,0)
--    'cameraZoom' cam 2 2      -- Zoom in
--    'cameraZoom' cam 2 1      -- Zoom out
-- @
--
--   <<docs/gifs/doc_cameraFocus.gif>>
cameraFocus :: Object s Camera -> V2 Double -> Scene s ()
cameraFocus cam new = do
  origin <- oRead cam oScaleOrigin
  t <- oRead cam oTranslate
  s <- oRead cam oScale
  let newLocation = new - ((new - origin) ^* s + origin - t)
  oModifyS cam $ do
    oTranslate .= newLocation
    oScaleOrigin .= new

-- | Instantaneously set camera zoom level.
cameraSetZoom :: Object s Camera -> Double -> Scene s ()
cameraSetZoom cam s =
  oModifyS cam $
    oScale .= s

-- | Change camera zoom level over a set duration.
cameraZoom :: Object s Camera -> Duration -> Double -> Scene s ()
cameraZoom cam d s =
  oTweenS cam d $ \t ->
    oScale %= \v -> fromToS v s t

-- | Instantaneously set camera location.
cameraSetPan :: Object s Camera -> V2 Double -> Scene s ()
cameraSetPan cam location =
  oModifyS cam $
    oTranslate .= location

-- | Change camera location over a set duration.
cameraPan :: Object s Camera -> Duration -> V2 Double -> Scene s ()
cameraPan cam d pos =
  oTweenS cam d $ \t ->
    oTranslate %= lerp t pos
