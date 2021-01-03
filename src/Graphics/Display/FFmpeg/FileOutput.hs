{-# LANGUAGE GADTs #-}

module Graphics.Display.FFmpeg.FileOutput (FileOutput(Unopened)) where

import Codec.FFmpeg (EncodingParams(EncodingParams), imageWriter, initFFmpeg)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Display.Class
import Graphics.Utils.ConversionUtils (toJuicyPixelImage)
import Graphics.Utils.Types

data FileOutput a where
  Unopened  :: Int -> Resolution -> FilePath -> FileOutput U
  Streaming :: (Maybe (V.Vector Word8) -> IO ()) -> Int -> Resolution -> FilePath -> FileOutput S

instance Show (FileOutput a) where
  show (Unopened fps res path) 
    = "Unopened: (fps: " ++ show fps ++ ", resolution: " ++ show res 
      ++ ", path:  " ++ path++ ")"
  show (Streaming _ fps res path) 
    = "Streaming: (fps: " ++ show fps ++ ", resolution: " ++ show res 
      ++ ", path:  " ++ path++ ")"

instance VideoOutput FileOutput where
  openDevice (Unopened fps res@(width, height) path) = do
    initFFmpeg
    encodingFun <- imageWriter params path
    return $ Streaming (encodingFun . (toJuicyPixelImage res <$>)) fps res path
    where
      params = EncodingParams (fromIntegral width) (fromIntegral height) 
                              fps Nothing Nothing "" Nothing

  writeFrame (Streaming writerFun _ _ _) 
    = writerFun . Just

  closeDevice (Streaming writerFun fps resolution path) = do
    writerFun Nothing 
    return $ Unopened fps resolution path

