#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Codec.Picture.Types
import           Control.Lens
import           Graphics.SvgTree                (Origin (..), PathCommand (..))
import           Linear.V2
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Builtin.Documentation
import           Reanimate.Scene

import           Objects

env :: Animation -> Animation
env = mapA (withStrokeColor "black")

main :: IO ()
main = reanimate $ env $ scene $ do
  newSpriteSVG_ $ mkBackground "grey"
  aPlot <- newObject $ ArcPlot
    { _arcPlotPartial = 1
    , _arcPlotFn      = promotePixel . viridis
    , _arcPlotAngle   = 120/180*pi
    , _arcPlotCenter  = 0.5
    , _arcPlotQuality = 100
    }
  rFn <- newObject $ RoundFunction 1 0.5
  sFn <- newObject $ SquareFunction 1 0.5 0.1 0.5
  oModify aPlot $
    oContext %~ \o -> scale 3 . o
  oModify rFn $
    oContext %~ \o -> lowerTransformations . scale 3 . o
  oModify sFn $
    oContext %~ \o -> lowerTransformations . scale 3 . o

  -- oShowWith rFn oDrawLine
  -- oShowWith aPlot oFadeIn
  -- oShowWith sFn $ adjustDuration (*2) . oDrawLine

  oShow rFn
  b1 <- newObject $ GoBoard 5 [] [] []
  oModify b1 $
    oContext %~ \o -> lowerTransformations . scale 2.5 . o
  oShow b1

  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotAngle %~ \v -> fromToS v (60/180*pi) t
  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotAngle %~ \v -> fromToS v (300/180*pi) t
  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotCenter %~ \v -> fromToS v 0.1 t
  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotCenter %~ \v -> fromToS v 0.9 t

  -- oTweenV aPlot 3 $ \t ->
  --   arcPlotPartial %~ \v -> fromToS v 1 t
  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotAngle %~ \v -> fromToS v (60/180*pi) t
  -- oTweenV aPlot 1 $ \t ->
  --   arcPlotPartial %~ \v -> fromToS v 0 t

  wait 1

oDrawLine :: SVG -> Animation
oDrawLine = oStagger $ \svg -> animate $ \t -> partialSvg t svg

