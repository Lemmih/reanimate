#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Combinators

main :: IO ()
main = reanimate $ autoReverse $ mkAnimation 5 $ \t ->
    let s = signalFromTo 0 1.5 (signalCurve 2) t in
    mkGroup
    [ mkBackground "black"
    , FilterTree $ mkFilter "blur"
      [FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num dev
        & filterResult .~ Just "blur"
      ] & filterWidth .~ pure (Percent 3)
        & filterX .~ pure (Percent (-1))
        & filterHeight .~ pure (Percent 3)
        & filterY .~ pure (Percent (-1))
    , FilterTree $ mkFilter "goo"
      [FEGaussianBlur $ defaultSvg
        & gaussianBlurStdDeviationX .~ Num dev
        & filterResult .~ Just "blur"
      ,FEColorMatrix $ defaultSvg
        & colorMatrixType .~ Matrix
        & colorMatrixValues .~ "1 0 0 0 0 \
                               \0 1 0 0 0 \
                               \0 0 1 0 0 \
                               \0 0 0 " ++ show (sharpness*2) ++ " -" ++ show sharpness
        & filterResult .~ pure "goo"
      ,FEComposite $ defaultSvg
        & compositeIn .~ pure SourceGraphic
        & compositeIn2 .~ pure (SourceRef "goo")
        & compositeOperator .~ CompositeAtop
      ] & filterWidth .~ pure (Percent 3)
        & filterX .~ pure (Percent (-1))
        & filterHeight .~ pure (Percent 3)
        & filterY .~ pure (Percent (-1))
    , translate 0 (radius*2.2) $ withFillColor "red" $ mkGroup
      [ translate (s*(-radius)) 0 circ
      , translate (s*radius) 0 circ
      ]
    , withFillColor "red" $ mkGroup
      [ translate (s*(-radius)) 0 circ
      , translate (s*radius) 0 circ
      ] & filterRef .~ pure (Ref "blur")
    , translate 0 (-radius*2.2) $ withFillColor "red" $ mkGroup
      [ translate (s*(-radius)) 0 circ
      , translate (s*radius) 0 circ
      ] & filterRef .~ pure (Ref "goo")
    ]
  where
    sharpness = 10
    dev = 0.2
    radius = 1
    circ = mkCircle (Num radius)

mkFilter :: String -> [FilterElement] -> Filter
mkFilter ident fe = defaultSvg & filterChildren .~ fe & attrId .~ Just ident
