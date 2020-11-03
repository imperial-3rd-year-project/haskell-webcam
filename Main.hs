module Main where

import Control.Monad (forever)
import Control.Concurrent (yield, threadDelay)
import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device
import qualified Data.Vector.Storable as V
import qualified Graphics.Display.Class as O
import qualified Graphics.Display.FFmpeg.FileOutput as O
import qualified Graphics.Display.V4L2.Device as D
import qualified Graphics.Display.DisplayBuffer as B

import Data.Word (Word8)
import Foreign.ForeignPtr

import System.IO (hPutBuf, withFile, IOMode(AppendMode), Handle)
import System.Exit (exitSuccess)

import Graphics.Display.WindowDisplay

main :: IO ()
main = do
  let deviceOutput = D.Unopened "/dev/video4"
  let bufDevOutput = B.newBuffer 3 deviceOutput
  streamingDevOutput <- O.openDevice bufDevOutput
  -- Output device setup
  let fileOutput = O.Unopened 30 (640, 360) "/tmp/video4.mp4"
      bufferedOutput = B.newBuffer 3 fileOutput

  streamingFileOutput   <- O.openDevice bufferedOutput 
  streamingWindowOutput <- O.openDevice (newOutputWindow (640, 360) 30)
   
  -- Input Device setup
  let device = Unopened "/dev/video0"
  opened <- openDevice device

  streaming <- startCapture opened $ \v -> O.writeFrame streamingWindowOutput v >> O.writeFrame streamingFileOutput v >> O.writeFrame streamingDevOutput v
  putStrLn "Hello, Haskell!"
  threadDelay 50000000

  opened <- stopCapture streaming
  closeDevice opened

  O.closeDevice streamingFileOutput

  exitSuccess
