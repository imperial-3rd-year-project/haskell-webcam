module Main where

import Control.Monad (forever)
import Control.Concurrent (yield, threadDelay)
import qualified Data.Vector.Storable as V
import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device
import qualified Graphics.Display.Class as O
import qualified Graphics.Display.FFmpeg.FileOutput as O

import Data.Word (Word8)
import Foreign.ForeignPtr

import System.IO (hPutBuf, withFile, IOMode(AppendMode), Handle)

main :: IO ()
main = do
  -- Output device setup
  let outputDevice = (O.Unopened 30 (640, 480) "/tmp/video4.mp4")
  streamingOutput <- O.openDevice outputDevice 
   
  -- Input Device setup
  let device = Unopened "/dev/video0"
  opened <- openDevice device

  streaming <- startCapture opened $ O.writeFrame streamingOutput
  putStrLn "Hello, Haskell!"
  threadDelay 5000000

  opened <- stopCapture streaming
  closeDevice opened

  O.closeDevice streamingOutput
  return ()
