module Main where

import Control.Concurrent (threadDelay)
import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device
import qualified Graphics.Display.Class as O
import qualified Graphics.Display.FFmpeg.FileOutput as O
import qualified Graphics.Display.DisplayBuffer as B

import System.Exit (exitSuccess)

import Graphics.Display.WindowDisplay

main :: IO ()
main = do
  -- Output device setup
  let fileOutput = (O.Unopened 30 (640, 480) "/tmp/video4.mp4")
      bufferedOutput = B.newBuffer 3 fileOutput

  streamingFileOutput   <- O.openDevice bufferedOutput 
  streamingWindowOutput <- O.openDevice (newOutputWindow (640, 480) 30)
   
  -- Input Device setup
  let device = Unopened "/dev/video0"
  opened <- openDevice device

  streaming <- startCapture opened $ \v -> O.writeFrame streamingWindowOutput v >> O.writeFrame streamingFileOutput v
  putStrLn "Hello, Haskell!"
  threadDelay 50000000

  opened <- stopCapture streaming
  closeDevice opened

  O.closeDevice streamingFileOutput

  exitSuccess
