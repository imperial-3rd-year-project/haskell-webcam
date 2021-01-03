{-# LANGUAGE GADTs #-}

module Graphics.Display.FFmpeg.FileOutput (FileOutput, newFileOutput) where

import Codec.FFmpeg (EncodingParams(EncodingParams), imageWriter, initFFmpeg)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Display.Class
import Graphics.Utils.ConversionUtils (toJuicyPixelImage)
import Graphics.Utils.Types

data FileOutput a where
  Unopened  :: Int -> Resolution -> FilePath -> FileOutput U
  Streaming :: Int -> Resolution -> FilePath -> (Maybe (V.Vector Word8) -> IO ()) -> FileOutput S

instance Show (FileOutput a) where
  show (Unopened fps res path) 
    = "Unopened: (fps: " ++ show fps ++ ", resolution: " ++ show res 
      ++ ", path:  " ++ path++ ")"
  show (Streaming fps res path _) 
    = "Streaming: (fps: " ++ show fps ++ ", resolution: " ++ show res 
      ++ ", path:  " ++ path ++ ")"

instance VideoOutput FileOutput where
  openDevice (Unopened fps res@(width, height) path) = do
    initFFmpeg
    encodingFun <- imageWriter params path
    return $ Streaming fps res path (encodingFun . (toJuicyPixelImage res <$>)) 
    where
      params = EncodingParams (fromIntegral width) (fromIntegral height) 
                              fps Nothing Nothing "" Nothing

  writeFrame (Streaming _ _ _ writerFun) 
    = writerFun . Just

  closeDevice (Streaming fps resolution path writerFun) = do
    writerFun Nothing 
    return $ Unopened fps resolution path

newFileOutput :: Int -> Resolution -> FilePath -> FileOutput U
newFileOutput = Unopened
