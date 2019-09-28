#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import           Graphics.SvgTree    hiding (Text)
import           Numeric
import           Reanimate.Animation
import           Reanimate.Constants
import           Reanimate.Driver    (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Signal
import           Reanimate.Svg

main :: IO ()
main = reanimate $ pauseAtEnd 5 $
    curvesExample (\_ -> ([], "[]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalFlat s)]
      , "[(1, signalFlat " <> ppD s <> ")]"))
      `before`
    curvesExample (\s ->
      ( [(s, signalFlat 0), (1, signalLinear)]
      , "[(" <> ppD s <> ", signalFlat 0), (1, signalLinear)]"))
      `before`
    curvesExample (\s ->
      ( [(s, signalFlat 1), (1, signalReverse signalLinear)]
      , "[(" <> ppD s <> ", signalFlat 0), (1, signalReverse signalLinear)]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalCurve (2+s*3))]
      , "[(1, signalCurve "<> ppD (2+s*3) <>")]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalFromTo s 1 $ signalCurve 5)]
      , "[(1, signalFromTo "<>  ppD s <>" 1 \\$ signalCurve 5)]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalBell (2+s*3))]
      , "[(1, signalBell "<> ppD (2+s*3)<>")]"))
  where
    ppD s = pack (showFFloat (Just 2) s "")

convertX, convertY :: Double -> Double
convertX x = x*(screenWidth/320)
convertY y = y*(screenHeight/180)

curvesExample :: (Double -> ([(Double, Double -> Double)], Text)) -> Animation
curvesExample gen = mkAnimation 2 $ \t ->
    mkGroup
    [ mkBackground "black"
    , withFillColor "white" $
      translate 0 (screenHeight*0.35) $
      center $ latex "Signals"
    , let (curveFns, name) = gen t in
      center $
      mkGroup
        [ withStrokeColor "white" $ withStrokeWidth 0.01 $
          mkGroup
          [ mkLine (0, 0)
                   (convertX 200, 0)
          , mkLine (0, 0)
                   (0, convertY 50) ]
        , withStrokeColor "white" $ withStrokeWidth 0.01 $
          mkGroup
          [ mkLine (0, convertX $ y)
                   (convertX 200, convertX y)
          | y <- [10,20,30,40,50] ]
        , withFillColor "white" $
          mkGroup
            [ translate (convertX $ -5) (convertX $ -5)  $ scale 0.5 $ center $ latex "0"
            , translate (convertX $ -5) (convertX $ 50)  $ scale 0.5 $ center $ latex "1"
            , translate (convertX $ 205) (convertX $ -5) $ scale 0.5 $ center $ latex "1"
            , translate (convertX $ 100) (convertX $ -30)$ scale 0.6 $ center $ latex name ]
        , withFillOpacity 0 $ withStrokeColor "green" $ -- withStrokeWidth 0.5 $
          lowerTransformations $ scaleXY (convertX $ 200) (convertX $ (50)) $ mkSignalLine (signalFromList curveFns)
        ]
    ]

mkSignalLine :: Signal -> Tree
mkSignalLine fn = mkLinePath
    [ (x/steps, fn (x/steps))
    | x <- [0 .. steps] ]
  where
    steps = 1000
