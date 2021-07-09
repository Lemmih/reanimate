#!/usr/bin/env stack
-- stack runghc
{-# LANGUAGE ParallelListComp #-}
module Main where

import           Control.Monad
import           Data.List
import qualified Data.Vector            as V
import           Debug.Trace
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Math.Polygon
import           Reanimate.Math.SSSP
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

{- GIFs:
 1. SSSP from a single point.
 2. SSSP from all points.
 3. Morph to tree.
-}

main :: IO ()
main = reanimate $ scene $ do
  newSpriteSVG_ $ mkBackground bgColor
  -- play $ ssspSingle
  -- play $ ssspMulti
  play $ ssspMorph
  -- wait 1

targetPolygon :: Polygon
targetPolygon = pAtCenter shape2

ssspSingle :: Animation
ssspSingle = pauseAtEnd 1 $ scene $ do
    newSpriteSVG_ $ ppPolygonOutline targetPolygon
    nodes <- newSpriteSVG $ mkGroup
      [ ppPolygonNodes targetPolygon
      , withFillColor rootColor $  ppPolygonNode targetPolygon 0 ]
    spriteZ nodes 1
    case t of
      T idx sub -> mapM_ (worker idx) sub
    wait 3
  where
    worker origin (T target sub) = do
      let V2 oX oY = realToFrac <$> pAccess targetPolygon origin
          V2 tX tY = realToFrac <$> pAccess targetPolygon target
      fork $ do
        wait 0.7
        dot <- fork $ newSpriteA' SyncFreeze $ animate $ \t -> mkGroup $
          [ translate tX tY $
            withFillColor pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
          ]
        spriteZ dot 2
        spriteE dot (overEnding 0.2 fadeOutE)
      l <- newSpriteA' SyncFreeze $ animate $ \t -> mkGroup $
        [ withStrokeColor pathColor $ partialSvg t $
          pathify $ mkLine (oX, oY) (tX, tY)
        , translate tX tY $
          withFillColor pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
        ]
      spriteE l (overEnding 0.2 fadeOutE)
      mapM_ (worker target) sub
    t = ssspTree targetPolygon

ssspMulti :: Animation
ssspMulti = mkAnimation 20 $ \t ->
  let p = pCycle targetPolygon t in
  mkGroup
  [ ppPolygonOutline p
  , ppSSSP p
  , ppPolygonNodes p
  , withFillColor rootColor $ ppPolygonNode p 0
  ]

ssspMorph :: Animation
ssspMorph =
    playThenReverseA $ pauseAround 1 3 $ signalA (curveS 2) $ mkAnimation 2 $ \t ->
      ppPolyGroup t (lerpPolygon t targetPolygon mPolygon)
  where
    ppPolyGroup t p = mkGroup
        [ withGroupOpacity (max 0.3 (1-t)) $ ppPolygonOutline p
        , ppSSSP p
        , ppPolygonNodes p
        , withFillColor rootColor $  ppPolygonNode p 0
        ]
    mPolygon = morphSSSP targetPolygon


------------------------------------------------------------------
-- Parameters

bgColor :: String
bgColor = "black"

outlineColor :: String
outlineColor = "grey"

pathColor :: String
pathColor = "red"

rootColor :: String
rootColor = "green"

nodeColor :: String
nodeColor = "white"

nodeRadius :: Double
nodeRadius = 0.2

------------------------------------------------------------------
-- Graphical utils

ppPolygonOutline :: Polygon -> SVG
ppPolygonOutline p = withFillOpacity 0 $ withStrokeColor outlineColor $
  withStrokeWidth (defaultStrokeWidth*3) $ mkLinePathClosed
  [ (x, y)
  | V2 x y <- map (fmap realToFrac) $ V.toList (polygonPoints p)
  ]

ppPolygonNodes :: Polygon -> SVG
ppPolygonNodes p = mkGroup $
  map (ppPolygonNode p) [0 .. pSize p-1]

ppPolygonNode :: Polygon -> Int -> SVG
ppPolygonNode p idx =
  withFillColor outlineColor $
  withStrokeWidth (defaultStrokeWidth*1) $
  withStrokeColor nodeColor $
  let V2 x y = realToFrac <$> pAccess p idx
  in translate x y $ mkCircle nodeRadius

ppSSSP :: Polygon -> SVG
ppSSSP p = mkGroup $
  map (ppPathTo p) [1 .. pSize p - 1]

ppPathTo :: Polygon -> Int -> SVG
ppPathTo p nth = withFillOpacity 0 $ withStrokeColor pathColor $ mkLinePath
    [ (x,y)
    | V2 x y <- map (fmap realToFrac . pAccess p) (reverse $ path nth)
    ]
  where
    t = polygonSSSP p V.! polygonOffset p
    path 0   = [0]
    path idx = idx : path (t V.! idx)

data T = T Int [T] deriving (Show)

ssspTree :: Polygon -> T
ssspTree p = worker 0
  where
    t = polygonSSSP p V.! polygonOffset p
    worker idx = T idx [ worker n | n <- [0 .. pSize p-1], t V.! n == idx, n /= idx]

morphSSSP :: Polygon -> Polygon
morphSSSP p = pAtCenter $
    p{ polygonPoints = V.generate (pSize p) $ \idx ->
        case lookup idx pos of
          Nothing -> error $ "invalid position: " ++ show idx
          Just (x, y, rowLength) ->
            let leftMost = negate ((rowLength-1)/2)
            in V2 (leftMost + x) (negate y)
      }
  where
    t = ssspTree p
    flat = flatten t
    pos =
      [ (elt, (x, y, fromIntegral $ length row))
      | (y, row) <- zip [0..] flat
      , (x, elt) <- zip [0..] row
      ]

flatten :: T -> [[Int]]
flatten (T n sub) = [n] : map concat (transpose (map flatten sub))

lerpPolygon :: Double -> Polygon -> Polygon -> Polygon
lerpPolygon t a b =
  a { polygonPoints = V.zipWith (lerp $ realToFrac t) (polygonPoints b) (polygonPoints a) }
