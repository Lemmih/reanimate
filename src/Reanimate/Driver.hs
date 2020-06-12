{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Reanimate.Driver
  ( reanimate
  )
where

import           Control.Applicative                      ( (<|>) )
import           Data.Maybe
import           Reanimate.Animation                      ( Animation )
import           Reanimate.Driver.Check
import           Reanimate.Driver.CLI
import           Reanimate.Driver.Compile
import           Reanimate.Driver.Server
import           Reanimate.Parameters
import           Reanimate.Render                         ( render
                                                          , renderSnippets
                                                          , renderSvgs
                                                          , selectRaster
                                                          )
import           System.Directory
import           System.FilePath
import           Text.Printf

presetFormat :: Preset -> Format
presetFormat Youtube    = RenderMp4
presetFormat ExampleGif = RenderGif
presetFormat Quick      = RenderMp4
presetFormat MediumQ    = RenderMp4
presetFormat HighQ      = RenderMp4
presetFormat LowFPS     = RenderMp4

presetFPS :: Preset -> FPS
presetFPS Youtube    = 60
presetFPS ExampleGif = 25
presetFPS Quick      = 15
presetFPS MediumQ    = 30
presetFPS HighQ      = 30
presetFPS LowFPS     = 10

presetWidth :: Preset -> Width
presetWidth Youtube    = 2560
presetWidth ExampleGif = 320
presetWidth Quick      = 320
presetWidth MediumQ    = 800
presetWidth HighQ      = 1920
presetWidth LowFPS     = presetWidth HighQ

presetHeight :: Preset -> Height
presetHeight preset = presetWidth preset * 9 `div` 16

formatFPS :: Format -> FPS
formatFPS RenderMp4  = 60
formatFPS RenderGif  = 25
formatFPS RenderWebm = 60

formatWidth :: Format -> Width
formatWidth RenderMp4  = 2560
formatWidth RenderGif  = 320
formatWidth RenderWebm = 2560

formatHeight :: Format -> Height
formatHeight RenderMp4  = 1440
formatHeight RenderGif  = 180
formatHeight RenderWebm = 1440

{-|
Main entry-point for accessing an animation. Creates a program that takes the
following command-line arguments:

> Usage: PROG [COMMAND]
>   This program contains an animation which can either be viewed in a web-browser
>   or rendered to disk.
>
> Available options:
>   -h,--help                Show this help text
>
> Available commands:
>   check                    Run a system's diagnostic and report any missing
>                            external dependencies.
>   view                     Play animation in browser window.
>   render                   Render animation to file.

Neither the 'check' nor the 'view' command take any additional arguments.
Rendering animation can be controlled with these arguments:

> Usage: PROG render [-o|--target FILE] [--fps FPS] [-w|--width PIXELS]
>                    [-h|--height PIXELS] [--compile] [--format FMT]
>                    [--preset TYPE]
>   Render animation to file.
>
> Available options:
>   -o,--target FILE         Write output to FILE
>   --fps FPS                Set frames per second.
>   -w,--width PIXELS        Set video width.
>   -h,--height PIXELS       Set video height.
>   --compile                Compile source code before rendering.
>   --format FMT             Video format: mp4, gif, webm
>   --preset TYPE            Parameter presets: youtube, gif, quick
>   -h,--help                Show this help text
-}
reanimate :: Animation -> IO ()
reanimate animation = do
  Options {..} <- getDriverOptions
  case optsCommand of
    Raw  -> setFPS 60 >> renderSvgs animation
    Test -> do
      setNoExternals True
      -- hSetBinaryMode stdout True
      renderSnippets animation
    Check       -> checkEnvironment
    View {..}   -> serve viewVerbose viewGHCPath viewOrigin
    Render {..} -> do
      let fmt =
            guessParameter renderFormat (fmap presetFormat renderPreset)
              $ case renderTarget of
                  -- Format guessed from output
                  Just target -> case takeExtension target of
                    ".mp4"  -> RenderMp4
                    ".gif"  -> RenderGif
                    ".webm" -> RenderWebm
                    _       -> RenderMp4
                  -- Default to mp4 rendering.
                  Nothing -> RenderMp4

      target <- case renderTarget of
        Nothing -> do
          self <- findOwnSource
          pure $ case fmt of
            RenderMp4  -> replaceExtension self "mp4"
            RenderGif  -> replaceExtension self "gif"
            RenderWebm -> replaceExtension self "webm"
        Just target -> makeAbsolute target

      let
        fps =
          guessParameter renderFPS (fmap presetFPS renderPreset) $ formatFPS fmt
        (width, height) = fromMaybe
          ( maybe (formatWidth fmt)  presetWidth  renderPreset
          , maybe (formatHeight fmt) presetHeight renderPreset
          )
          (userPreferredDimensions renderWidth renderHeight)

      if renderCompile
        then compile
          [ "render"
          , "--fps"
          , show fps
          , "--width"
          , show width
          , "--height"
          , show height
          , "--format"
          , showFormat fmt
          , "--raster"
          , showRaster renderRaster
          , "--target"
          , target
          , "+RTS"
          , "-N"
          , "-RTS"
          ]
        else do
          raster <- selectRaster renderRaster
          setRaster raster
          setFPS fps
          setWidth width
          setHeight height
          printf
            "Animation options:\n\
                 \  fps:    %d\n\
                 \  width:  %d\n\
                 \  height: %d\n\
                 \  fmt:    %s\n\
                 \  target: %s\n\
                 \  raster: %s\n"
            fps
            width
            height
            (showFormat fmt)
            target
            (show raster)

          render animation target raster fmt width height fps

guessParameter :: Maybe a -> Maybe a -> a -> a
guessParameter a b def = fromMaybe def (a <|> b)


-- If user specifies exactly one dimension explicitly, calculate the other
userPreferredDimensions :: Maybe Width -> Maybe Height -> Maybe (Width, Height)
userPreferredDimensions (Just width) (Just height) = Just (width, height)
userPreferredDimensions (Just width) Nothing =
  Just (width, makeEven $ width * 9 `div` 16)
userPreferredDimensions Nothing (Just height) =
  Just (makeEven $ height * 16 `div` 9, height)
userPreferredDimensions Nothing Nothing = Nothing

-- Avoid ffmpeg failures "height not divisible by 2"
makeEven :: Int -> Int
makeEven x | even x    = x
           | otherwise = x - 1
