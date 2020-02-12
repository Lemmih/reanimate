#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Scene

-- This should give rect.
main :: IO ()
main = reanimate $ signalA (constantS 1) $ sceneAnimation $ do
  play $ animate $ const $ mkCircle 1
  play $ animate $ const $ mkRect 1 1
