{-# LANGUAGE MultiWayIf #-}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Internal tools for rastering SVGs and rendering videos. You are unlikely
to ever directly use the functions in this module.

-}
module Reanimate.Render
  ( render
  , renderSvgs
  , renderSvgs_
  , renderSnippets        -- :: Animation -> IO ()
  , renderLimitedFrames
  , Format(..)
  , Raster(..)
  , Width, Height, FPS
  , requireRaster         -- :: Raster -> IO Raster
  , selectRaster          -- :: Raster -> IO Raster
  , applyRaster           -- :: Raster -> FilePath -> IO ()
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad             (forM_, forever, unless, void, when)
import           Data.Either               (isRight)
import           Data.Function             (fix)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Data.Time
import           Graphics.SvgTree          (Number (..))
import           Numeric                   (showFFloat)
import           Reanimate.Animation       (Animation, duration, frameAt, renderSvg)
import           Reanimate.Driver.Check    (hasInkscape, hasMagick, hasRSvg)
import           Reanimate.Driver.Magick   (magickCmd)
import           Reanimate.Misc
import           Reanimate.Parameters
import           System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import           System.Directory          (doesFileExist, removeFile)
import           System.Exit               (ExitCode (ExitFailure), exitWith)
import           System.FileLock           (SharedExclusive (..), unlockFile, withTryFileLock)
import           System.FilePath           (replaceExtension, (<.>), (</>))
import           System.IO
import           System.IO.Temp            (createTempDirectory, getCanonicalTemporaryDirectory)
import           Text.Printf               (printf)

idempotentFile :: FilePath -> IO () -> IO ()
idempotentFile path action = do
    _ <- withTryFileLock lockFile Exclusive $ \lock -> do
      haveFile <- doesFileExist path
      unless haveFile action
      unlockFile lock
      _ <- try (removeFile lockFile) :: IO (Either SomeException ())
      return ()
    return ()
  where
    lockFile = path <.> "lock"

-- | Generate SVGs at 60fps and put them in a folder.
renderSvgs :: FilePath -> Int -> Bool -> Animation -> IO ()
renderSvgs folder offset _prettyPrint ani = do
  print frameCount
  lock <- newMVar ()
  handle errHandler $ concurrentForM_ (frameOrder rate frameCount) $ \nth' -> do
    let nth = (nth'+offset) `mod` frameCount
        now = (duration ani / (fromIntegral frameCount - 1)) * fromIntegral nth
        frame = frameAt (if frameCount <= 1 then 0 else now) ani
        path = folder </> show nth <.> "svg"
        svg = renderSvg Nothing Nothing frame

    idempotentFile path $
      writeFile path svg
    withMVar lock $ \_ -> do
      print nth
      hFlush stdout
 where
  rate       = 60
  frameCount = round (duration ani * fromIntegral rate) :: Int
  errHandler (ErrorCall msg) = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)

renderSvgs_ :: Animation -> (Int -> FilePath -> IO ()) -> IO ()
renderSvgs_ ani cb = do
  tmp <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory tmp "reanimate"
  lock <- newMVar ()
  handle errHandler $ concurrentForM_ (frameOrder rate frameCount) $ \nth' -> do
    let nth = (nth') `mod` frameCount
        now = (duration ani / (fromIntegral frameCount - 1)) * fromIntegral nth
        frame = frameAt (if frameCount <= 1 then 0 else now) ani
        path = tmpDir </> show nth <.> "svg"
        svg = renderSvg Nothing Nothing frame

    idempotentFile path $
      writeFile path svg
    withMVar lock $ \_ -> cb nth path
 where
  rate       = 60
  frameCount = round (duration ani * fromIntegral rate) :: Int
  errHandler (ErrorCall msg) = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)

-- | Render as many frames as possible in 2 seconds. Limited to 20 frames.
renderLimitedFrames :: FilePath -> Int -> Bool -> Int -> Animation -> IO ()
renderLimitedFrames folder offset _prettyPrint rate ani = do
    now <- getCurrentTime
    worker (addUTCTime timeLimit now) frameLimit (frameOrder rate frameCount)
  where
    timeLimit = 2
    frameLimit = 20 :: Int
    worker _ 0 _ = return ()
    worker _ _ [] = putStrLn "Done"
    worker localTimeLimit l (x:xs) = do
      curTime <- getCurrentTime
      if curTime > localTimeLimit
        then return ()
        else do
          let nth = (x+offset) `mod` frameCount
              now = (duration ani / (fromIntegral frameCount - 1)) * fromIntegral nth
              frame = frameAt (if frameCount <= 1 then 0 else now) ani
              svg = renderSvg Nothing Nothing frame
              path = folder </> show nth <.> "svg"
              tmpPath = path <.> "tmp"
          haveFile <- doesFileExist path
          if haveFile
            then worker localTimeLimit l xs
            else do
              writeFile tmpPath svg
              renameOrCopyFile tmpPath path
              print nth
              worker localTimeLimit (l-1) xs
    frameCount = round (duration ani * fromIntegral rate) :: Int

-- XXX: Merge with 'renderSvgs'
-- | Render 10 frames and print them to stdout. Used for testing.
--
--   XXX: Not related to the snippets in the playground.
renderSnippets :: Animation -> IO ()
renderSnippets ani = forM_ [0 .. frameCount - 1] $ \nth -> do
  let now   = (duration ani / (fromIntegral frameCount - 1)) * fromIntegral nth
      frame = frameAt now ani
      svg   = renderSvg Nothing Nothing frame
  putStr (show nth)
  T.putStrLn $ T.concat . T.lines . T.pack $ svg
  where frameCount = 10 :: Integer

frameOrder :: Int -> Int -> [Int]
frameOrder fps nFrames = worker [] fps
 where
  worker _seen 0        = []
  worker seen  nthFrame = filterFrameList seen nthFrame nFrames
    ++ worker (nthFrame : seen) (nthFrame `div` 2)

filterFrameList :: [Int] -> Int -> Int -> [Int]
filterFrameList seen nthFrame nFrames = filter (not . isSeen)
                                               [0, nthFrame .. nFrames - 1]
  where isSeen x = any (\y -> x `mod` y == 0) seen

-- | Video formats supported by reanimate.
data Format = RenderMp4 | RenderGif | RenderWebm
  deriving (Show)

mp4Arguments :: FPS -> FilePath -> FilePath -> FilePath -> [String]
mp4Arguments fps progress template target =
  [ "-r"
  , show fps
  , "-i"
  , template
  , "-preset"
  , "veryslow"
  , "-y"
  , "-c:v"
  , "libx264"
  , "-tune"
  , "animation"
  , "-vf"
  , "fps=" ++ show fps
  , "-crf"
  , "23"
  , "-movflags"
  , "+faststart"
  , "-progress"
  , progress
  , "-pix_fmt"
  , "yuv420p"
  , target
  ]

-- gifArguments :: FPS -> FilePath -> FilePath -> FilePath -> [String]
-- gifArguments fps progress template target =

-- | Render animation to a video file with given parameters.
render
  :: Animation
  -> FilePath
  -> Raster
  -> Format
  -> Width
  -> Height
  -> FPS
  -> Bool
  -> IO ()
render ani target raster format width height fps partial = do
  printf "Starting render of animation: %.1f\n" (duration ani)
  ffmpeg <- requireExecutable "ffmpeg"
  generateFrames raster ani width height fps partial $ \template ->
    withTempFile "txt" $ \progress -> do
      writeFile progress ""
      progressH <- openFile progress ReadMode
      hSetBuffering progressH NoBuffering
      allFinished <- newEmptyMVar
      void $ forkIO $ do
        progressPrinter "rendered" (animationFrameCount ani fps)
          $ \done -> fix $ \loop -> do
            eof <- hIsEOF progressH
            if eof
              then threadDelay 1000000 >> loop
              else do
                l <- try (hGetLine progressH)
                case l of
                  Left  SomeException{} -> return ()
                  Right str             ->
                    case take 6 str of
                      "frame=" -> do
                        void $ swapMVar done (read (drop 6 str))
                        loop
                      _ | str == "progress=end" -> return ()
                      _                         -> loop
        putMVar allFinished ()
      case format of
        RenderMp4 -> runCmd ffmpeg (mp4Arguments fps progress template target)
        RenderGif -> withTempFile "png" $ \palette -> do
          runCmd
            ffmpeg
            [ "-i"
            , template
            , "-y"
            , "-vf"
            , "fps="
            ++ show fps
            ++ ",scale="
            ++ show width
            ++ ":"
            ++ show height
            ++ ":flags=lanczos,palettegen"
            , "-t"
            , showFFloat Nothing (duration ani) ""
            , palette
            ]
          runCmd
            ffmpeg
            [ "-framerate"
            , show fps
            , "-i"
            , template
            , "-y"
            , "-i"
            , palette
            , "-progress"
            , progress
            , "-filter_complex"
            , "fps="
            ++ show fps
            ++ ",scale="
            ++ show width
            ++ ":"
            ++ show height
            ++ ":flags=lanczos[x];[x][1:v]paletteuse"
            , "-t"
            , showFFloat Nothing (duration ani) ""
            , target
            ]
        RenderWebm -> runCmd
          ffmpeg
          [ "-r"
          , show fps
          , "-i"
          , template
          , "-y"
          , "-progress"
          , progress
          , "-c:v"
          , "libvpx-vp9"
          , "-vf"
          , "fps=" ++ show fps
          , target
          ]
      takeMVar allFinished

---------------------------------------------------------------------------------
-- Helpers

progressPrinter :: String -> Int -> (MVar Int -> IO ()) -> IO ()
progressPrinter typeName maxCount action = do
  printf "\rFrames %s: 0/%d" typeName maxCount
  putStr $ clearFromCursorToLineEndCode ++ "\r"
  done  <- newMVar (0 :: Int)
  start <- getCurrentTime
  let bgThread = forever $ do
        nDone <- readMVar done
        now   <- getCurrentTime
        let spent = diffUTCTime now start
            remaining =
              (spent / (fromIntegral nDone / fromIntegral maxCount)) - spent
        printf "\rFrames %s: %d/%d" typeName nDone maxCount
        putStr $ ", time spent: " ++ ppDiff spent
        unless (nDone == 0) $ do
          putStr $ ", time remaining: " ++ ppDiff remaining
          putStr $ ", total time: " ++ ppDiff (remaining + spent)
        putStr $ clearFromCursorToLineEndCode ++ "\r"
        hFlush stdout
        threadDelay 1000000
  withBackgroundThread bgThread $ action done
  now <- getCurrentTime
  let spent = diffUTCTime now start
  printf "\rFrames %s: %d/%d" typeName maxCount maxCount
  putStr $ ", time spent: " ++ ppDiff spent
  putStr $ clearFromCursorToLineEndCode ++ "\n"

animationFrameCount :: Animation -> FPS -> Int
animationFrameCount ani rate = round (duration ani * fromIntegral rate) :: Int

generateFrames
  :: Raster -> Animation -> Width -> Height -> FPS -> Bool -> (FilePath -> IO a) -> IO a
generateFrames raster ani width_ height_ rate partial action = withTempDir $ \tmp -> do
  let frameName nth = tmp </> printf nameTemplate nth
  setRootDirectory tmp
  progressPrinter "generated" frameCount
    $ \done -> handle h $ concurrentForM_ frames $ \n -> do
        writeFile (frameName n) $ renderSvg width height $ nthFrame n
        modifyMVar_ done $ \nDone -> return (nDone + 1)

  when (isValidRaster raster)
    $ progressPrinter "rastered" frameCount
    $ \done -> handle h $ concurrentForM_ frames $ \n -> do
        applyRaster raster (frameName n)
        modifyMVar_ done $ \nDone -> return (nDone + 1)

  action (tmp </> rasterTemplate raster)
 where
  isValidRaster RasterNone = False
  isValidRaster RasterAuto = False
  isValidRaster _          = True

  width  = Just $ Px $ fromIntegral width_
  height = Just $ Px $ fromIntegral height_
  h UserInterrupt | partial = do
    hPutStrLn
      stderr
      "\nCtrl-C detected. Trying to generate video with available frames. \
                       \Hit ctrl-c again to abort."
    return ()
  h other = throwIO other
  -- frames = [0..frameCount-1]
  frames = frameOrder rate frameCount
  nthFrame nth = frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
  frameCount = animationFrameCount ani rate
  nameTemplate :: String
  nameTemplate = "render-%05d.svg"

withBackgroundThread :: IO () -> IO a -> IO a
withBackgroundThread t = bracket (forkIO t) killThread . const

ppDiff :: NominalDiffTime -> String
ppDiff diff | hours == 0 && mins == 0 = show secs ++ "s"
            | hours == 0              = printf "%.2d:%.2d" mins secs
            | otherwise               = printf "%.2d:%.2d:%.2d" hours mins secs
 where
  (osecs, secs) = round diff `divMod` (60 :: Int)
  (hours, mins) = osecs `divMod` 60

rasterTemplate :: Raster -> String
rasterTemplate RasterNone = "render-%05d.svg"
rasterTemplate RasterAuto = "render-%05d.svg"
rasterTemplate _          = "render-%05d.png"

-- | Resolve RasterNone and RasterAuto. If no valid raster can
--   be found, exit with an error message.
requireRaster :: Raster -> IO Raster
requireRaster raster = do
  raster' <- selectRaster (if raster == RasterNone then RasterAuto else raster)
  case raster' of
    RasterNone -> do
      hPutStrLn
        stderr
        "Raster required but none could be found. \
        \Please install either inkscape, imagemagick, or rsvg-convert."
      exitWith (ExitFailure 1)
    _ -> pure raster'

-- | Resolve RasterNone and RasterAuto. If no valid raster can
--   be found, return RasterNone.
selectRaster :: Raster -> IO Raster
selectRaster RasterAuto = do
  rsvg   <- hasRSvg
  ink    <- hasInkscape
  magick <- hasMagick
  if
    | isRight rsvg   -> pure RasterRSvg
    | isRight ink    -> pure RasterInkscape
    | isRight magick -> pure RasterMagick
    | otherwise      -> pure RasterNone
selectRaster r = pure r

-- | Convert SVG file to a PNG file with selected raster engine. If
--   raster engine is RasterAuto or RasterNone, do nothing.
applyRaster :: Raster -> FilePath -> IO ()
applyRaster RasterNone     _    = return ()
applyRaster RasterAuto     _    = return ()
applyRaster RasterInkscape path = runCmd
  "inkscape"
  [ "--without-gui"
  , "--file=" ++ path
  , "--export-png=" ++ replaceExtension path "png"
  ]
applyRaster RasterRSvg path = runCmd
  "rsvg-convert"
  [path, "--unlimited", "--output", replaceExtension path "png"]
applyRaster RasterMagick path =
  runCmd magickCmd [path, replaceExtension path "png"]

concurrentForM_ :: [a] -> (a -> IO ()) -> IO ()
concurrentForM_ lst action = do
  n    <- getNumCapabilities
  sem  <- newQSemN n
  eVar <- newEmptyMVar
  forM_ lst $ \elt -> do
    waitQSemN sem 1
    emp <- isEmptyMVar eVar
    if emp
      then
        void
          $ forkIO
              (         catch       (action elt) (void . tryPutMVar eVar)
              `finally` signalQSemN sem          1
              )
      else signalQSemN sem 1
  waitQSemN sem n
  mbE <- tryTakeMVar eVar
  case mbE of
    Nothing -> return ()
    Just e  -> throwIO (e :: SomeException)
