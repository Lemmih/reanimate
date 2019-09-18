#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Main (main) where

import           Codec.Picture.Types
import           Control.Monad       (forM_)
import           Data.Fixed          (mod')
import           Graphics.SvgTree    (Number (..), Tree)
import           Reanimate.Driver    (reanimate)
import           Reanimate.Effect
import           Reanimate.LaTeX     (latex)
import           Reanimate.Monad
import           Reanimate.Scene
import           Reanimate.Signal    (signalLinear)
import           Reanimate.Svg

main :: IO ()
main = reanimate $ mkAnimation 0 (emit $ mkBackground "black") `sim`
  mainScene

mainScene :: Animation
mainScene = sceneAnimation $ mdo
    play $ drawCircle
      # setDuration drawCircleT
      # applyE (constE $ scaleXY (-1) 1)
    fork $ play $ drawCircle
      # freezeAtPercentage 1
      # setDuration rotDur
    rotDur <- withSceneDuration $ waitAll $
      forM_ svgs $ \svg -> do
        fork $ play $ drawTick
          # setDuration rotateT
          # repeatAnimation rotateN
          # applyE (overBeginning 0.5 drawInE)
          # applyE (overEnding 0.5 drawOutE)
        fork $ play $ drawSVG svg
          # setDuration rotateT
          # repeatAnimation rotateN
          # applyE (overBeginning rotateT drawInE)
          # applyE (delayE rotateT $ overBeginning 1 fillInE)
          # applyE (overEnding 0.5 fadeOutE)
        wait (rotateT / fromIntegral (1+length svgs))
    play $ drawCircle
      # setDuration drawCircleT
      # reverseAnimation
    return ()
  where
    drawCircleT = 1
    rotateT     = 5
    rotateN     = 3

    svg = center $ latex "\\LaTeX"
    getNth n = snd (splitGlyphs [n] svg)
    svgs = [
        withStrokeWidth (Num 0.01) $ 
        scale 2 $
        translate 0 (tickLength*2) $
        withStrokeColor "white" $
        withFillColor "white" $
        center $ getNth n
      | n <- [0..4]]

radius = 1.25
tickLength = 0.25

drawCircle :: Animation
drawCircle = mkAnimation 1 $ do
  n <- getSignal signalLinear
  emit $
    withFillOpacity 0 $
    withStrokeColor "white" $
    rotate (-90) $
    partialSvg n circPath
  where
    circPath = pathify $ mkCircle (Num radius)

drawTick :: Animation
drawTick = drawSVG $ mkLine (Num 0, Num 0) (Num 0, Num $ tickLength)

drawSVG :: Tree -> Animation
drawSVG t = mkAnimation 1 $ do
  n <- getSignal signalLinear
  emit $
    withStrokeColor "white" $
    rotate (n*360) $
    translate 0 radius $
    t
