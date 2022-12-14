module Main where

import Control.Monad (forever)
import Control.Concurrent (yield, threadDelay)
import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device 
import Graphics.Display.ConversionUtils (centredOffset, resize)
import Graphics.Display.V4L2.Device (newV4L2Output)
import qualified Data.Vector.Storable as V
import qualified Graphics.Display.Class as O
import qualified Graphics.Display.FFmpeg.FileOutput as O
import qualified Graphics.Display.DisplayBuffer as B

import Data.Word (Word8)
import Foreign.ForeignPtr

import System.IO (hPutBuf, withFile, IOMode(AppendMode), Handle)
import System.Exit (exitSuccess)

import Graphics.Display.WindowDisplay

transform :: V.Vector Word8 -> V.Vector Word8
transform = V.map (255 -) 

demoWriteToDevice :: IO ()
demoWriteToDevice = do
  let deviceOutput = newV4L2Output v4l2resolution "/dev/video4"
  let bufDevOutput = B.newBuffer 3 deviceOutput
  streamingDevOutput <- O.openDevice bufDevOutput
  -- Output device setup
  let fileOutput = O.Unopened 30 v4l2resolution "/tmp/video4.mp4"
      --bufferedOutput = B.newBuffer 3 fileOutput

  streamingFileOutput   <- O.openDevice fileOutput
  streamingWindowOutput <- O.openDevice (newOutputWindow v4l2resolution 30)

   
  -- Input Device setup
  let device = newV4L2CaptureDevice "/dev/video0"
  opened <- openDevice device

  let input2 = newV4L2CaptureDevice "/dev/video4"
  inputOpened <- openDevice input2


--  streaming <- startCapture opened $ \v -> O.writeFrame streamingWindowOutput v >> O.writeFrame streamingFileOutput v -- >> O.writeFrame streamingDevOutput v
  streamingCamera <- startCapture opened $ \v -> O.writeFrame streamingDevOutput (transform v)
  streamingDev <- startCapture inputOpened $ \v -> O.writeFrame streamingWindowOutput v >> O.writeFrame streamingFileOutput v

  -- streaming <- startCapture opened $ \v -> O.writeFrame streamingDevOutput v
  putStrLn "Hello, Haskell!"
  threadDelay 15000000

  -- opened <- stopCapture streaming
  cameraOpened <- stopCapture streamingCamera
  O.closeDevice streamingFileOutput
  devOpened <- stopCapture streamingDev
  putStrLn "here"
  closeDevice cameraOpened
  putStrLn "here"
  closeDevice devOpened


  exitSuccess

main :: IO ()
main = do
  let device = newV4L2CaptureDevice "/dev/video0"
  let printResolution = (800, 800)
  opened <- openDevice device
  let displayOutput = newOutputWindow printResolution 30
  let bufDisplayOutput = B.newBuffer 3 displayOutput
  -- let fileOutput = O.Unopened 30 printResolution "/tmp/video4.mp4"
  -- streamingFileOutput <- O.openDevice fileOutput
  streamingWindowOutput <- O.openDevice bufDisplayOutput

  let offset = centredOffset v4l2resolution printResolution

  streamingCamera <- startCapture opened 
    $ \v -> O.writeFrame streamingWindowOutput (resize offset v4l2resolution printResolution v)

  threadDelay 150000000

  cameraOpened <- stopCapture streamingCamera
  --O.closeDevice streamingFileOutput
  closeDevice cameraOpened

  exitSuccess
