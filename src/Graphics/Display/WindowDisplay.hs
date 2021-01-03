{-# LANGUAGE GADTs #-}

module Graphics.Display.WindowDisplay 
  ( WindowDisplayOutput
  , newOutputWindow
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBQueue
import Control.Monad.STM

import Codec.Picture

import qualified Data.Vector.Storable as V
import Data.Word (Word8)

import Foreign.ForeignPtr (ForeignPtr)

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Simulate

import Graphics.Display.Class
import Graphics.Utils.ConversionUtils


data WindowDisplayOutput a where
  Unopened :: (Int, Int) -> Int -> WindowDisplayOutput U
  Streaming :: (Int, Int) -> Int -> (V.Vector Word8 -> IO ()) -> WindowDisplayOutput S


instance VideoOutput WindowDisplayOutput where
  openDevice = openWindowOutput

  writeFrame (Streaming _ _ w) = w

  closeDevice _ = error "GLUT backend does not support closing"

newOutputWindow :: (Int, Int) -> Int -> WindowDisplayOutput U
newOutputWindow = Unopened

openWindowOutput :: WindowDisplayOutput U -> IO (WindowDisplayOutput S)
openWindowOutput (Unopened res fps) = Streaming res fps <$> newPreviewWindow
  where
    newPreviewWindow :: IO (V.Vector Word8 -> IO ())
    newPreviewWindow = do
      queue <- newTBQueueIO 2

      _ <- forkIO $ do
        firstFrame <- atomically $ readTBQueue queue
        simulateIO (InWindow "Webcam" res (0, 0)) black fps (frameToPicture firstFrame) return (\_ _ -> readFrame queue)

      return (atomically . writeTBQueue queue)
  
    readFrame :: TBQueue (V.Vector Word8) -> Picture -> IO Picture
    readFrame queue oldPic = atomically $ last . (oldPic :) . (frameToPicture <$>) <$> flushTBQueue queue

    foreignPtrFromImage :: Image PixelRGBA8 -> (ForeignPtr Word8, Int, Int)
    foreignPtrFromImage (Image w h vec) = (let (ptr, _) = V.unsafeToForeignPtr0 vec in ptr, w, h)

    frameToPicture :: V.Vector Word8 -> Picture
    frameToPicture frame =
      let rgb8Img = toJuicyPixelImage res frame
          rgbaImg = convertRGBA8 (ImageRGB8 rgb8Img)
          (ptr, w, h) = foreignPtrFromImage rgbaImg
       in bitmapOfForeignPtr w h (BitmapFormat TopToBottom PxRGBA) ptr False
