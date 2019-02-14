{-# LANGUAGE OverloadedStrings, Arrows #-}
module Reanimate.Arrow where

import Control.Arrow
import qualified Control.Category as C
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Data.Fixed
import Lucid.Svg
import Lucid ()

type Duration = Double
type Time = Double

data Animation a b = Animation Duration (Duration -> Time -> a -> Svg b)

instance C.Category Animation where
  id = Animation 0 (\_ _ -> pure)
  Animation a fn1 . Animation b fn2 = Animation (max a b) (\d t a -> fn2 d t a >>= fn1 d t)

instance Arrow Animation where
  -- arr :: (b -> c) -> Animation b c
  arr fn = Animation 0 (\d t -> pure . fn)
  -- first :: Animation b c -> Animation (b, d) (c, d)
  first (Animation duration fn) =
    Animation duration (\t dur (b,d) -> do c <- fn t dur b; pure (c, d))

type Ani a = Animation () a

duration :: Double -> Animation a ()
duration duration = Animation duration (\_ _ _ -> pure ())

defineAnimation :: Animation a b -> Animation a b
defineAnimation (Animation d fn) = Animation d (\_ t -> fn d (t `mod'` d))

animationDuration :: Animation a b -> Double
animationDuration (Animation d _) = d

frameAt :: Double -> Ani () -> Svg ()
frameAt n (Animation d fn) = svg $ fn d (n `mod'` d) ()
  where
    svg :: Svg () -> Svg ()
    svg content = do
      doctype_
      with (svg11_ content) [width_ "320" , height_ "180", viewBox_ "0 0 320 180"]

emit :: Animation (Svg ()) ()
emit = Animation 0 (\d t svg -> svg)

getTime :: Ani Double
getTime = Animation 0 (\d t () -> pure t)

getDuration :: Ani Duration
getDuration = Animation 0 (\d t () -> pure d)
