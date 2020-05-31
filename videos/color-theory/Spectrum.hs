{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ApplicativeDo     #-}
module Spectrum
  ( scene2
  ) where

import           Control.Lens                    ((&), (.~))

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import qualified Data.ByteString                 as BS
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV        (hsvView)
import           Data.Colour.SRGB
import           Data.Colour.SRGB.Linear
import           Data.List
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text                       (Text)
import           Graphics.SvgTree                hiding (Text)
import           Linear.V2
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Builtin.CirclePlot
import           Reanimate.Builtin.Documentation
import qualified Reanimate.Builtin.TernaryPlot   as Ternary
import           Reanimate.ColorComponents
import           Reanimate.ColorSpace
import           Reanimate.Scene
import           System.IO.Unsafe

import Transcript

{- STORYBOARD
 - Wavelength scene
    * Background: wavelength/sensitivity grid
    * Draw SML sensitivities with labels
    * Hide labels
    * Morph to XYZ sensitivities
    * Label as XYZ
-}

long = [ (nm, l) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
medium = [ (nm, m) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
short = [ (nm, s) | (nm, (l, m, s)) <- Map.toList coneSensitivity ]
xCoords = [ (nm, x/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
yCoords = [ (nm, y/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]
zCoords = [ (nm, z/2) | (nm, (x, y, z)) <- Map.toList bigXYZCoordinates ]

labScaleX = 110 -- 100 -- 128
labScaleY = 110 -- 100 -- 128

blueName = "royalblue"
greenName = "green"
redName = "maroon"

drawSensitivities :: Scene s ()
drawSensitivities = spriteScope $ do
    -- bg <- newSpriteSVG $ spectrumGrid True
    -- spriteZ bg 1

    waitOn $ forM_ [(short, blueName), (medium, greenName), (long, redName)] $
      \(dat,name) -> do
        fork $ newSpriteA' SyncFreeze $ drawSensitivity dat name
          # setDuration drawDur
        wait drawStagger
    wait 1
  where
    drawDur = 3
    drawStagger = 1

-- drawMorphingSensitivities :: Animation
-- drawMorphingSensitivities = sceneAnimation $ do
--     bg <- newSpriteSVG $ spectrumGrid True
--     spriteZ bg 1

--     forM_ keys $ \(datA, datB, name) -> do
--       fork $ newSpriteA $ morphSensitivity datA datB name
--         # setDuration drawDur
--   where
--     keys = [ (short, zCoords, blueName)
--            , (medium, yCoords, greenName)
--            , (long, xCoords, redName)]
--     drawDur = 3

drawLabelS = drawLabelSVG "S" blueName
drawLabelM = drawLabelSVG "M" greenName
drawLabelL = drawLabelSVG "L" redName

drawLabelZ = drawLabelSVG "Z" blueName
drawLabelY = drawLabelSVG "Y" greenName
drawLabelX = drawLabelSVG "X" redName

illustrateSpectrum :: Scene s ()
illustrateSpectrum = spriteScope $ do
    intensity <- newSpriteSVG spectrumGridIntensity
    spriteE intensity $ overEnding 0.2 fadeOutE
    grid <- newSpriteSVG $ spectrumGrid False
    spriteZ grid 1
    waitUntil $ wordStart $ findWord ["theory"] "spectrum"
    forM_ (zip [0 ..] spectrum) $ \(nth, intensity) -> do
      let nm = fromIntegral nth * (300/23/2) + 400
      fork $ drawLine nm intensity
      wait 0.05
    waitUntil $ wordEnd $ findWord ["simplify"] "sensing"
  where
    spectrum =
        [ 0.02, 0.03, 0.07, 0.1, 0.2, 0.35, 0.7, 0.8, 0.9 ,0.75, 0.5 ] ++
        [ 0.3, 0.27, 0.31, 0.25, 0.3, 0.35, 0.4, 0.5] ++
        [ 0.65, 0.8, 0.95, 1.0, 0.95, 0.98, 0.93, 0.97] ++
        [ 0.85, 0.75, 0.65, 0.60, 0.55, 0.52, 0.48, 0.45] ++
        [ 0.45, 0.40, 0.35, 0.30, 0.25, 0.20, 0.15, 0.15, 0.15, 0.13, 0.11, 0.08]
    drawLine nm intensity = do
      let Just c = fmap (promotePixel.toRGB8) (nmToColor (round nm))
      s <- newSpriteSVG $
        pathify $
        withStrokeColorPixel c $
        translate (-spectrumWidth/2) (-spectrumHeight/2) $
        translate (fromToS 0 spectrumWidth ((nm-400)/(700-400))) 0 $
        mkLine (0,0) (0, spectrumHeight*intensity)
      spriteE s $ overEnding 0.3 fadeOutE
      -- spriteTween s intensity $ partialSvg . curveS 2
      spriteTween s 0.8 $ partialSvg . curveS 2

scene2 :: Scene s ()
scene2 = spriteScope $ do
  illustrateSpectrum
  oldGrid <- newSpriteSVG $ spectrumGrid False
  newGrid <- newSpriteSVG $ spectrumGrid True
  spriteZ newGrid 1
  spriteTween newGrid 0.5 withGroupOpacity
  destroySprite oldGrid
  
  (longL, longD) <- plotLineSprite newGrid redName
  writeVar longD long

  (mediumL, mediumD) <- plotLineSprite newGrid greenName
  writeVar mediumD medium

  (shortL, shortD) <- plotLineSprite newGrid blueName
  writeVar shortD short

  labelS <- do
    waitUntil $ wordStart $ findWord ["simplify"] "short"
    fork $ tweenVar shortL 3 $ \v -> fromToS v 1
    newSpriteSVG $ drawLabelS
  spriteMap labelS $ uncurry translate (labelPosition short)

  labelM <- fork $ do
    waitUntil $ wordStart $ findWord ["simplify"] "medium"
    fork $ tweenVar mediumL 3 $ \v -> fromToS v 1
    newSpriteSVG $ drawLabelM
  spriteMap labelM $ uncurry translate (labelPosition medium)

  labelL <- fork $ do
    waitUntil $ wordStart $ findWord ["simplify"] "long"
    fork $ tweenVar longL 3 $ \v -> fromToS v 1
    newSpriteSVG $ drawLabelL
  spriteMap labelL $ uncurry translate (labelPosition long)

  forM_ [labelS, labelM, labelL] $ \label -> do
    spriteE label $ overBeginning 0.3 fadeInE
    spriteE label $ overEnding 0.3 fadeOutE

  waitUntil $ wordStart $ findWord ["xyz"] "stretched"
  -- Drop SML labels
  forM_ [labelS, labelM, labelL] destroySprite

  waitOn $ do
    now <- queryNow
    let dur = wordEnd (findWord ["xyz"] "squeezed") - now
    fork $ tweenVar shortD dur  $ \v -> morphSensitivity v zCoords . curveS 2
    fork $ tweenVar mediumD dur $ \v -> morphSensitivity v yCoords . curveS 2
    fork $ tweenVar longD dur   $ \v -> morphSensitivity v xCoords . curveS 2

  -- XYZ labels and timings
  labelZ <- fork $ newSpriteSVG $ drawLabelZ
  spriteE labelZ $ overBeginning 0.3 fadeInE
  labelZPos <- spriteVar labelZ (labelPosition zCoords) $ uncurry translate

  labelY <- fork $ newSpriteSVG $ drawLabelY
  spriteE labelY $ overBeginning 0.3 fadeInE
  labelYPos <- spriteVar labelY (labelPosition yCoords) $ uncurry translate

  labelX <- fork $ newSpriteSVG $ drawLabelX
  spriteE labelX $ overBeginning 0.3 fadeInE
  labelXPos <- spriteVar labelX (labelPosition xCoords) $ uncurry translate

  waitUntil $ wordStart $ findWord ["xyz"] "2D"

  fork $ spriteTween newGrid 2 $ \t -> scale (fromToS 1 0.5 $ curveS 2 t)
  fork $ spriteTween newGrid 2 $ \t -> translate (fromToS 0 (screenWidth/4) $ curveS 2 t) 0

  fork $ tweenVar labelZPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 0
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5-1) s)

  fork $ tweenVar labelYPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 1 0
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5) s)

  fork $ tweenVar labelXPos 2 $ \(x,y) t ->
    let (newX, newY) = Ternary.toOffsetCartesianCoords 0 1
        s = curveS 2 t
    in (fromToS x (newX*5-screenWidth/4) s, fromToS y (newY*5-1) s)

  wait 2

  redFactor <- newVar 0
  greenFactor <- newVar 0
  blueFactor <- newVar 0
  xyzSpace <- newSprite $
    cieXYImage
      <$> unVar redFactor
      <*> unVar greenFactor
      <*> unVar blueFactor
      <*> pure imgSize
  spriteMap xyzSpace $ translate (-screenWidth/4) 0
  spriteZ xyzSpace (-1)
  
  waitUntil $ wordStart $ findWord ["pyramid"] "pyramid"
  spriteTween xyzSpace 0.5 $ aroundCenter . scale . curveS 2

  wait 1


  waitUntil $ wordStart $ findWord ["pyramid"] "reds"
  spriteE labelX (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar redFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "greens"
  spriteE labelY (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar greenFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "blues"
  spriteE labelZ (overBeginning 1 $ aroundCenterE highlightE)
  tweenVar blueFactor 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord ["pyramid"] "filter"

  let downShift = 1
  visCurve <- newVar 0
  waveLine <- fork $ newSpriteSVG $
    translate (screenWidth/4) 0 $
    scale 0.5 $
    translate 0 (-spectrumHeight/2) $
    wavelengthAxis
  spriteTween waveLine 1 $ \d ->
    translate 0 (fromToS 0 (-downShift) $ curveS 2 d)
  -- wait 1
  visLine <- newSprite $
    withStrokeWidth 0.03 . withStrokeColor "white" . morphXYZCoordinates
      <$> unVar visCurve
  spriteZ visLine (1)

  spriteTween waveLine 0.5 $ \t -> withGroupOpacity (1-t)
  destroySprite waveLine

  visSide <- spriteVar visLine 0 $ \t ->
    translate 0 (-downShift) .
    translate (fromToS (screenWidth/4) (-screenWidth/4) $ curveS 2 t) 0 .
    scale (fromToS 0.5 1 $ curveS 2 t) .
    translate 0 (fromToS (-spectrumHeight/2) 0 $ curveS 2 t)
  fork $ tweenVar visSide 3 $ \v -> fromToS v 1 . curveS 2
  fork $ tweenVar visCurve 3 $ \v -> fromToS v 1 . curveS 2
  fork $ spriteTween visLine 3 $ \d ->
    translate 0 (fromToS 0 downShift $ curveS 2 d)

  obsVisible <- newVar 1
  gamut <- newVar 0
  reorient <- newVar 0
  cmOpacity <- newVar 1
  cmDelta <- newVar 0
  cmName <- newVar "sinebow"
  cmFunc <- newVar sinebow
  visSpace <- fork $ newSprite $ do
    getObs <- unVar obsVisible
    getGamut <- unVar gamut
    getReorient <- unVar reorient
    getOpacity <- unVar cmOpacity
    getDelta <- unVar cmDelta
    getFunc <- unVar cmFunc
    return $
      translate (-screenWidth/4) 0 $
      mkGroup
      [ mkClipPath "visible"
        [ simplify
          obsColors
        ]
      , mkClipPath "sRGB"
        [ simplify $
          sRGBTriangle getGamut
        ]
      , withGroupOpacity getObs $
        withClipPathRef (Ref "visible") $
        mkGroup [cieXYImage 1 1 1 imgSize]
      , translate 0 (1*getReorient) $
        rotate (-gamutSlope sRGBGamut * getReorient) $
        mkGroup
        [ scale (1+0.5*getReorient) $
          withClipPathRef (Ref "sRGB") $
          mkGroup [cieXYImageGamut getGamut imgSize]
        , lowerTransformations $ scale (1+0.5*getReorient) $
          withGroupOpacity getOpacity $
          cmToTernary getDelta getFunc
        ]
      , withGroupOpacity getReorient $
        translate 0 3.5 $ scale 0.5 $
        center $ withFillColor "white" $
        latex "sRGB"
      ]

  colorMap <- newSprite $ do
    getOpacity <- unVar cmOpacity
    getDelta <- unVar cmDelta
    getFunc <- unVar cmFunc
    getName <- unVar cmName
    return $
      withGroupOpacity getOpacity $
      mkGroup
      [ renderColorMap getDelta (screenWidth*0.75) (screenHeight*0.15) getFunc
      , withGroupOpacity (getDelta * 2) $
        withFillColor "white" $
        translate 0 1.2 $
        scale 0.7 $
        center $ latex getName
      ]
  spriteTween colorMap 0 $ const $ translate 0 (-3)

  wait 4
  fork $ spriteTween visLine 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween xyzSpace 1 $ \t -> withGroupOpacity (1-t)
  wait 2

  fork $ spriteTween newGrid 0.5 $ \t -> withGroupOpacity (1-t)

  fork $ tweenVar labelXPos 1 $ \(x,y) t ->
    let (newX, newY) = (-1,3)
        s = curveS 2 t
    in (fromToS x newX s, fromToS y newY s)
  fork $ tweenVar labelYPos 1 $ \(x,y) t ->
    let (newX, newY) = (0,3)
        s = curveS 2 t
    in (fromToS x newX s, fromToS y newY s)
  fork $ tweenVar labelZPos 1 $ \(x,y) t ->
    let (newX, newY) = (1,3)
        s = curveS 2 t
    in (fromToS x newX s, fromToS y newY s)

  spriteZ visSpace (-1)

  spriteTween visSpace 1 $ \t ->
    translate (fromToS 0 (screenWidth/4) $ curveS 2 t) 0

  waitUntil $ wordStart $ findWord ["rgb"] "triangle"

  rgb <- newSprite $ withStrokeColor "white" . sRGBTriangle <$> unVar gamut
  spriteTween rgb 1 $ partialSvg
  
  waitUntil $ wordStart $ findWord ["rgb"] "Finally"
  fork $ spriteTween rgb 1 $ \t -> withGroupOpacity (1-t)

  fork $ spriteTween labelX 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween labelY 1 $ \t -> withGroupOpacity (1-t)
  fork $ spriteTween labelZ 1 $ \t -> withGroupOpacity (1-t)

  tweenVar obsVisible 1 $ \v -> fromToS v 0 . curveS 2

  tweenVar reorient 1 $ \v -> fromToS v 1 . curveS 2

  waitUntil $ wordStart $ findWord [] "path"

  writeVar cmName "jet"
  writeVar cmFunc jet
  tweenVar cmDelta 1 $ \d -> fromToS d 1

  waitUntil $ wordStart $ findWord ["draw"] "Hue-Saturation-Value"

  spriteTween visSpace 1 $ \t -> translate (-2.5*curveS 2 t) 0

  hsv <- newSprite $ do
    getOpacity <- unVar cmOpacity
    getDelta <- unVar cmDelta
    getFunc <- unVar cmFunc
    return $
      mkGroup
      [ lowerTransformations $
        scaleToWidth (screenWidth*0.20) $
        mkGroup [ hsvColorSpace 100
                , withGroupOpacity getOpacity $
                  cmToHSV getDelta getFunc]
      , translate 0 2 $ scale 0.5 $
        center $ withFillColor "white" $
        latex "HSV" ]

    -- lchColorSpace 100
    -- cieLABImage 100 50
    -- cieLABImage 2000 2000
  spriteTween hsv 0 $ const $ translate 3 1.5
  spriteTween hsv 1 withGroupOpacity

  waitUntil $ wordStart $ findWord ["hsv"] "draw"

  tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
  writeVar cmName "sinebow"
  writeVar cmFunc sinebow
  writeVar cmOpacity 1
  writeVar cmDelta 0
  tweenVar cmDelta 5 $ \d -> fromToS d 1


  waitUntil $ wordStart $ findWord ["lab"] "LAB"
  wait (-2)

  fork $ spriteTween hsv 1 $ \t -> translate (2*curveS 2 t) 0
  spriteTween visSpace 1 $ \t -> translate (-2*curveS 2 t) 0

  lab <- newSprite $ do
    getOpacity <- unVar cmOpacity
    getDelta <- unVar cmDelta
    getFunc <- unVar cmFunc
    return $
      mkGroup
      [ lowerTransformations $
        scaleToWidth (screenWidth/4) $
        mkGroup [ cieLABImagePixels
                , withGroupOpacity getOpacity $
                  cmToLAB getDelta getFunc]
      , translate 0 2 $ scale 0.5 $
        center $ withFillColor "white" $
        latex "LAB" ]

  spriteTween lab 0 $ const $ translate 0 1.5
  spriteTween lab 1 withGroupOpacity

  waitUntil $ wordStart $ findWord ["made"] "Parula"
  tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
  writeVar cmName "parula"
  writeVar cmFunc parula
  writeVar cmOpacity 1
  writeVar cmDelta 0
  tweenVar cmDelta 5 $ \d -> fromToS d 1

  waitUntil $ wordStart $ findWord ["made"] "Viridis"
  tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
  writeVar cmName "viridis"
  writeVar cmFunc viridis
  writeVar cmOpacity 1
  writeVar cmDelta 0
  tweenVar cmDelta 5 $ \d -> fromToS d 1

  waitUntil $ wordStart $ findWord ["made"] "Cividis"
  tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
  writeVar cmName "cividis"
  writeVar cmFunc cividis
  writeVar cmOpacity 1
  writeVar cmDelta 0
  tweenVar cmDelta 5 $ \d -> fromToS d 1

  waitUntil $ wordStart $ findWord ["made"] "Turbo"
  tweenVar cmOpacity 0.3 $ \o t -> fromToS o 0 t
  writeVar cmName "turbo"
  writeVar cmFunc turbo
  writeVar cmOpacity 1
  writeVar cmDelta 0
  tweenVar cmDelta 5 $ \d -> fromToS d 1

  waitUntil $ wordEnd $ findWord ["grid"] "Looking"
  where
    imgSize = 50
    obsColors =
      lowerTransformations $
      scale 5 $
      renderXYZCoordinatesTernary


renderColorMap :: Double -> Double -> Double -> (Double -> PixelRGB8) -> Tree
renderColorMap delta width height cmap =
  translate (-width/2 * (1-delta)) 0 $
  mkGroup
  [ scaleToSize (width*delta) height $ showColorMap (\t -> cmap (t*delta))
  , withStrokeWidth (defaultStrokeWidth*0.7) $
    withStrokeColor "white" $ withFillOpacity 0 $
    mkRect (width*delta) height
  ]

renderXYZCoordinatesTernary :: Tree
renderXYZCoordinatesTernary =
  withFillOpacity 0 $
  mkLinePath $
  [ (x, y)
  | (_nm, (red,green,blue)) <- Map.toList lightXYZCoordinates
  , let (x,y) = Ternary.toOffsetCartesianCoords green red
  ]

morphXYZCoordinates :: Double -> Tree
morphXYZCoordinates t = withFillOpacity 0 $
  mkLinePath $
  [ (fromToS linearX (x*5) t, fromToS 0 (y*5) t)
  | (nm, (red,green,blue)) <- Map.toList lightXYZCoordinates
  , let (x,y) = Ternary.toOffsetCartesianCoords green red
  , let percent = (fromIntegral nm-initNM)/(lastNM-initNM)
        linearX = percent * spectrumWidth - spectrumWidth/2
  ]
  where
    dat = Map.toList lightXYZCoordinates
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700 -- fromIntegral $ fst (last dat)

-- Red corner: 0.64 0.33 0
-- Green corner: 0.3 0.6 0.1
-- Blue corner: 0.15 0.06 0.79
--1 0 0 -> 0.64 0.33 0.0
--0 1 0 -> 0.30 0.60 0.1
cieXYImageGamut :: Double -> Int -> Tree
cieXYImageGamut t density = Ternary.ternaryPlot density $ \aCoord bCoord cCoord ->
    let
      aCoord' = fromToS aCoord (rX * aCoord + gX * bCoord + bX * cCoord) t
      bCoord' = fromToS bCoord (rY * aCoord + gY * bCoord + bY * cCoord) t
      cCoord' = fromToS cCoord (rZ * aCoord + gZ * bCoord + bZ * cCoord) t
      RGB r g b = toSRGBBounded (cieXYZ aCoord' bCoord' cCoord')
    in PixelRGBA8 r g b 0xFF
  where
    RGB r g b = primaries sRGBGamut
    (rX, rY, rZ) = chromaCoords $ chromaConvert r
    (gX, gY, gZ) = chromaCoords $ chromaConvert g
    (bX, bY, bZ) = chromaCoords $ chromaConvert b

-- slope in degrees
gamutSlope :: RGBGamut -> Double
gamutSlope gamut = atan2 (y1/y2) (x1/x2) / pi * 180
  where
    RGB r g b = primaries sRGBGamut
    (gX, gY, _gZ) = chromaCoords $ chromaConvert g
    (bX, bY, _bZ) = chromaCoords $ chromaConvert b
    (x1, y1) = Ternary.toCartesianCoords gX gY
    (x2, y2) = Ternary.toCartesianCoords bX bY

strokeLine :: Double -> [(Double, Double)] -> SVG
strokeLine t points = mkGroup
  [ withStrokeWidth (defaultStrokeWidth*1) $
    withFillOpacity 0 $ withStrokeColor "black" $
    partialSvg t $
    mkLinePath points
      & strokeLineCap .~ pure CapRound
  , withStrokeWidth (defaultStrokeWidth*0.5) $
    withFillOpacity 0 $ withStrokeColor "white" $
    partialSvg t $ mkLinePath points
      & strokeLineCap .~ pure CapRound
  ]

-- 0.15 0.06 -> 0 0
cmToTernary :: Double -> (Double -> PixelRGB8) -> Tree
cmToTernary 0 _ = mkGroup []
cmToTernary t cm =
    lowerTransformations $ scale 5 $
    strokeLine t points
  where
    steps = 100
    points =
      [ Ternary.toOffsetCartesianCoords (cieY/s) (cieX/s)
      | n <- [0..steps]
      , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
            (cieX,cieY,cieZ) = cieXYZView (sRGB24 red green blue)
            s = cieX + cieY + cieZ
      ]

cmToHSV :: Double -> (Double -> PixelRGB8) -> Tree
cmToHSV t cm =
    lowerTransformations $ scale (screenHeight/2) $
    strokeLine t points
  where
    steps = 100
    points =
      [ (cos radian * s, sin radian * s)
      | n <- [0..steps]
      , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
            (h,s,_v) = hsvView (toSRGB $ sRGB24 red green blue)
            radian = h/180*pi
      ]

-- dim = 100
-- -labScaleX = 0
-- 0 = dim/2
-- +labScaleX = dim
-- -labScaleX to +labScaleX
cmToLAB :: Double -> (Double -> PixelRGB8) -> Tree
cmToLAB t cm =
  lowerTransformations $
  translate (-screenHeight/2) (-screenHeight/2) $
  strokeLine t  points
  where
    steps = 100
    points =
      [ ( (a+labScaleX)/(labScaleX*2)*screenHeight,
          (b+labScaleY)/(labScaleY*2)*screenHeight )
      | n <- [0..steps]
      , let PixelRGB8 red green blue = cm (fromIntegral n / fromIntegral steps)
            (_l,a,b) = cieLABView d65 (sRGB24 red green blue)
      ]


-- aCoord = red
-- bCoord = green
-- cCoord = blue
-- closer to white when (aCoord+cCoord) approaches 0
cieXYImage :: Double -> Double -> Double -> Int -> Tree
cieXYImage redFactor greenFactor blueFactor density =
    Ternary.ternaryPlot density $ \aCoord bCoord cCoord ->
    let
      white = chromaColour d65 1
      c = cieXYZ aCoord bCoord cCoord
      redD = fromToS (1-aCoord) 1 redFactor
      greenD = fromToS (1-bCoord) 1 greenFactor
      blueD = fromToS (1-cCoord) 1 blueFactor
      RGB r g b = toSRGBBounded $
        blend (1-redD) white $
        blend (1-greenD) white $
        blend (1-blueD) white $ c
      -- RGB r g b = toSRGBBounded (chromaColour d65 1)
    in PixelRGBA8 r g b 0xFF

cieLABImagePixels :: SVG
cieLABImagePixels = scaleToHeight screenHeight $ embedImage $
  unsafePerformIO $ do
    dat <- BS.readFile "lab.png"
    case decodePng dat of
      Left err  -> error err
      Right img -> return $ convertRGBA8 img

cieLABImages :: Int -> Tree
cieLABImages dim = mkGroup
  [ cieLABImage dim lStar
  | lStar <- [40..95]
  ]

cieLABImage :: Int -> Double -> Tree
cieLABImage dim = embedImage . cieLABImage' dim

cieLABImage' dim lStar = generateImage gen dim dim
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral dim) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral dim)) * labScaleY*2 - labScaleY
          -- lStar = 50 -- findLStar aStar bStar
          color = cieLAB d65 lStar aStar bStar
          RGB r g b = toSRGBBounded (cieLAB d65 lStar aStar bStar)
          -- RGB r g b = RGB (round $ lStar/100 * 255) (round $ lStar/100 * 255) (round $ lStar/100 * 255)
      in if inGamut sRGBGamut color
          then PixelRGBA8 r g b 0xFF
          else PixelRGBA8 0xFF 0xFF 0xFF 0x00

cieLABImage_ :: Int -> Tree
cieLABImage_ = embedImage . cieLABImage_'

cieLABImage_' dim = generateImage gen dim dim
  where
    gen x y =
      let
          aStar = (fromIntegral x / fromIntegral dim) * labScaleX*2 - labScaleX
          bStar = (1-(fromIntegral y / fromIntegral dim)) * labScaleY*2 - labScaleY
          -- lStar = 50 -- findLStar aStar bStar
          colors = [ toSRGBBounded color
                   | lStar <- reverse [40 .. 95]
                   , let color = cieLAB d65 lStar aStar bStar
                   , inGamut sRGBGamut color ]
      in case listToMaybe colors of
           Nothing          -> PixelRGBA8 0xFF 0xFF 0xFF 0x00
           Just (RGB r g b) -> PixelRGBA8 r g b 0xFF

findLStar :: Double -> Double -> Double
findLStar aStar bStar = worker 0 100 10
  where
    -- worker minL maxL n | trace (show (minL, maxL, n)) False = undefined
    worker minL maxL 0 = minL + (maxL-minL)/2
    worker minL maxL n
      | total > 1 = worker minL thisL (n-1)
      | otherwise = worker thisL maxL (n-1)
      where
        thisL = minL + (maxL-minL)/2
        (x,y,z) = cieXYZView (cieLAB d65 thisL aStar bStar)
        total = x+y+z

colorMapToLABCoords :: (Double -> PixelRGB8) -> Tree
colorMapToLABCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (aStar, 1-bStar)
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (lStar, aStar, bStar) = cieLABView d65 (sRGB24 r g b)
    ]
  where
    steps = 100

colorMapToXYZCoords :: (Double -> PixelRGB8) -> Tree
colorMapToXYZCoords colorMap = withFillOpacity 0 $ mkLinePath
    [ (x/s, 1-(y/s))
    | n <- [0 .. steps]
    , let PixelRGB8 r g b = colorMap (n/steps)
          (x, y, z) = cieXYZView (sRGB24 r g b)
          s = x+y+z
    ]
  where
    steps = 100

sRGBTriangle :: Double -> Tree
sRGBTriangle t =
    lowerTransformations $
    scale 5 $
    withFillOpacity 0 $
    mkClosedLinePath
    [ Ternary.toOffsetCartesianCoords (fromToS rY 0 t) (fromToS rX 1 t)
    , Ternary.toOffsetCartesianCoords (fromToS gY 1 t) (fromToS gX 0 t)
    , Ternary.toOffsetCartesianCoords (fromToS bY 0 t) (fromToS bX 0 t) ]
  where
    RGB r g b = primaries sRGBGamut
    (rX, rY, _) = chromaCoords $ chromaConvert r
    (gX, gY, _) = chromaCoords $ chromaConvert g
    (bX, bY, _) = chromaCoords $ chromaConvert b

mkClosedLinePath :: [(Double, Double)] -> Tree
mkClosedLinePath [] = mkGroup []
mkClosedLinePath ((startX, startY):rest) =
    PathTree $ defaultSvg & pathDefinition .~ cmds
      & strokeLineJoin .~ pure JoinRound
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ]
           , EndPath ]

spectrumHeight = screenHeight * 0.5
spectrumWidth = screenWidth * 0.7


wavelengthAxis :: Tree
wavelengthAxis =
  withStrokeWidth strokeWidth $
  mkGroup
  [ translate (-spectrumWidth/2) 0 $
    withFillOpacity 0 $ withStrokeColor "white" $ mkPath $
    [ MoveTo OriginAbsolute [V2 0 0]
    , VerticalTo OriginRelative [tickLength,-tickLength]
    , HorizontalTo OriginRelative [spectrumWidth]
    , VerticalTo OriginRelative [tickLength,-tickLength]
    ]
    ++ concat
    [ [ MoveTo OriginAbsolute [V2 (n / (nTicksX-1) * spectrumWidth) 0]
      , VerticalTo OriginRelative [tickLength]]
    | n <- [0..nTicksX-1]
    ]
  ]
  where
    strokeWidth = 0.03
    nTicksX = 24
    nTicksY = fromIntegral (round (spectrumHeight/spectrumWidth * nTicksX))
    tickLength = spectrumHeight*0.02

spectrumGridIntensity :: SVG
spectrumGridIntensity =
    withFillColor "white" $
    translate (-spectrumWidth*0.5 - svgWidth svg*0.7) 0 $
    svg
  where
    svg =
      center $ scale 0.7 $ rotate 90 $
      latex "Intensity $\\rightarrow$"

spectrumGrid :: Bool -> Tree
spectrumGrid includeSensitivity =
  withStrokeWidth strokeWidth $
  mkGroup
  [ --center $
    translate (-spectrumWidth/2) (spectrumHeight/2) $
    withFillOpacity 0 $ withStrokeColor "white" $ mkPath $
    [ MoveTo OriginAbsolute [V2 0 0]
    , HorizontalTo OriginRelative [tickLength,-tickLength]
    , VerticalTo OriginRelative [-spectrumHeight]
    , HorizontalTo OriginRelative [spectrumWidth]
    , VerticalTo OriginRelative [tickLength,-tickLength]
    ]
    ++ concat
    [ [ MoveTo OriginAbsolute [V2 (n / (nTicksX-1) * spectrumWidth) (-spectrumHeight)]
      , VerticalTo OriginRelative [tickLength]]
    | n <- [0..nTicksX-1]
    ]
    ++ concat
    [ [ MoveTo OriginAbsolute [V2 0 (n / (nTicksY-1) * negate spectrumHeight)]
      , HorizontalTo OriginRelative [tickLength]]
    | n <- [0..nTicksY-1]
    ]
  , if includeSensitivity
    then withFillColor "white" $
         translate (-spectrumWidth*0.5 - svgWidth sensitivity*0.7) 0 $
         sensitivity
    else None
  , withFillColor "white" $
    translate 0 (-spectrumHeight*0.5 - svgHeight wavelength*0.7) $
    wavelength
  , withFillColor "white" $
    translate (-spectrumWidth*0.5 + svgWidth shortWaves*0.5)
              (-spectrumHeight*0.5 - svgHeight shortWaves*0.7) $
    shortWaves
  , withFillColor "white" $
    translate (spectrumWidth*0.5 + svgWidth longWaves*0.5)
              (-spectrumHeight*0.5 - svgHeight longWaves*0.7) $
    longWaves
  ]
  where
    strokeWidth = 0.03
    nTicksX = 24
    nTicksY = fromIntegral (round (spectrumHeight/spectrumWidth * nTicksX))
    tickLength = spectrumHeight*0.02
    sensitivity =
      center $
      scale 0.7 $ rotate 90 $
      latex "Sensitivity $\\rightarrow$"
    wavelength =
      center $
      scale 0.8 $
      latex "Wavelength"
    shortWaves =
      rotate (-45) $ center $
      scale 0.3 $
      latex "400 nm"
    longWaves =
      rotate (-45) $ center $
      scale 0.3 $
      latex "700 nm"


drawSensitivity :: [(Nanometer, Double)] -> String -> Animation
drawSensitivity dat c = animate $ \limit ->
  sensitivitySVG limit dat c

morphSensitivity
  :: [(Nanometer, Double)]
  -> [(Nanometer, Double)]
  -> Double
  -> [(Nanometer, Double)]
morphSensitivity datA datB m =
  [ (nm, fromToS a b m)
  | ((nm,a),(_,b)) <- zip datA datB ]

plotLineSprite :: Sprite s -> String -> Scene s (Var s Double, Var s [(Nanometer, Double)])
plotLineSprite parent color = do
  limitV <- newVar 0
  datV <- newVar []
  spriteModify parent $ do
    l <- unVar limitV
    d <- unVar datV
    return $ \(svg,z) -> (mkGroup [sensitivitySVG l d color, svg], z)
  return (limitV, datV)

sensitivitySVG :: Double -> [(Nanometer, Double)] -> String -> Tree
sensitivitySVG limit dat c =
    mkGroup
    [ mkClipPath "spectrum" $
      let margin = 1 in
      [ simplify $ lowerTransformations $
        translate 0 (margin/2) $
        pathify $
        mkRect spectrumWidth (spectrumHeight+margin)
      ]
    ,
    withClipPathRef (Ref "spectrum") $
    simplify $ lowerTransformations $
    withStrokeColor c $
    withFillOpacity 0 $
    translate (-spectrumWidth/2) (-spectrumHeight/2) $
    mkPath $ MoveTo OriginAbsolute [V2 0 0] :
      [ LineTo OriginAbsolute [V2 x y]
      | (nm, n) <- dat
      , fromIntegral nm <= lastNM
      , let percent = (fromIntegral nm-initNM)/(lastNM-initNM)
            x = percent * spectrumWidth
            y = n/maxHeight * spectrumHeight
      , percent <= limit
      ] ]
  where
    maxHeight = 1
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700 -- fromIntegral $ fst (last dat)

labelPosition :: [(Nanometer, Double)] -> (Double, Double)
labelPosition dat =
    ( spectrumWidth*percent - spectrumWidth/2
    , labelY - spectrumHeight/2)
  where
    (nm,v) = maximumBy (comparing snd) dat
    labelY = v * spectrumHeight
    initNM = fromIntegral $ fst (head dat)
    lastNM = 700
    percent = (fromIntegral nm-initNM)/(lastNM-initNM)

drawLabelSVG :: Text -> String -> SVG
drawLabelSVG label c =
    translate (0) (svgHeight labelSVG * 0.7) $
    withFillColor c $
    labelSVG
  where
    labelSVG =
      center $
      scale 1 $
      latex label

moveUp :: SVG -> SVG
moveUp svg = translate 0 (-svgHeight svg * 1.5) svg

moveDown :: SVG -> SVG
moveDown svg = translate 0 (svgHeight svg * 1.5) svg

interpolation :: Animation
interpolation = mkAnimation 2 $ \t ->
    mkGroup
    [ translate 0 3 $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 hsvComponents a b
    , translate 0 1 $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 xyzComponents a b
    , translate 0 (-1) $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 lchComponents a b
    , translate 0 (-3) $
      scaleXY (curveS 2 1) 0.2 $
      showColorMap $ interpolateRGB8 labComponents a b
    ]
  where
    a = cyan -- green
    b = red
    yellow = PixelRGB8 0xFF 0xFF 0x00
    green = PixelRGB8 0x00 0xFF 0x00
    blue = PixelRGB8 0x00 0x00 0xFF
    cyan = PixelRGB8 0x00 0xFF 0xFF
    red =  PixelRGB8 0xFF 0x00 0x00

lchColorSpace :: Int -> Tree
lchColorSpace width =
  circlePlot width $ \ang radius ->
    let
        toRad deg = deg/180 * pi
        h = ang/pi*180
        aStar = (cos (toRad h) * c)
        bStar = (sin (toRad h) * c)
        l = 50 -- findLStar aStar bStar
        c = fromToS 0 (sqrt (labScaleX^2 + labScaleY^2)) radius
        -- RGB r g b = toSRGBBounded (colorPack lchComponents l c h)
        color = cieLAB d65 l aStar bStar
        RGB r g b = toSRGBBounded color
    in if inGamut sRGBGamut color
      then PixelRGBA8 r g b 0xFF
      else PixelRGBA8 0 0 0 0x00

hsvColorSpace :: Int -> Tree
hsvColorSpace width =
  circlePlot width $ \ang radius ->
    let
        h = ang/pi*180
        v = 1
        s = radius
        color = colorPack hsvComponents h s v
        RGB r g b = toSRGBBounded color
    in if inGamut sRGBGamut color
      then PixelRGBA8 r g b 0xFF
      else PixelRGBA8 0 0 0 0x00

spacesA :: Animation
spacesA = mkAnimation 10 $ \t ->
  scaleToSize screenWidth screenHeight $
  if t < 0.3
    then cieLABImage 100 50
    else if t < 0.6
      then lchColorSpace 100
      else hsvColorSpace 100

highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t/d)*0.5) . rotate (wiggleS (t/d) * 20)

-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t*4)
  | t < 0.75  = sin ((t-0.25)*2*pi+pi/2)
  | otherwise = curveS 2 ((t-0.75)*4)-1
