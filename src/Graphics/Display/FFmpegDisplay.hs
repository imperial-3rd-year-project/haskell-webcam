module Graphics.Display.FFmpegDisplay where

import Codec.FFmpeg (EncodingParams(EncodingParams), imageWriter, initFFmpeg)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Graphics.Display.ConversionUtils (toJuicyPixelImage)

-- Returns a function to be used with startCapture recording 
-- the camera output to file specified by the file path.
--
-- Returned function usage: 
-- Normal image frame should be wrapped in a Just.
-- When nothing is sent, the capture is stopped (and the thread 
-- creating this video)
toFileDisplay :: FilePath -> (Int, Int) -> Int -> IO (Maybe (V.Vector Word8) -> IO ())
toFileDisplay path dims@(width, height) fps = do
  initFFmpeg
  encodingFun <- imageWriter params path
  return $ encodingFun . (toJuicyPixelImage dims <$>) 
  where
    params = EncodingParams (fromIntegral width) (fromIntegral height) 
                            fps Nothing Nothing "" Nothing

