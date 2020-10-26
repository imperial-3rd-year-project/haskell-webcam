module Graphics.Capture.Class (VideoCapture (..)) where

import Data.Vector.Storable
import Data.Word
import Graphics.Utils.Types (U, O, S)

class VideoCapture device where
  getDevices        :: IO [device U]
  openDevice        :: device U -> IO (device O)
  closeDevice       :: device O -> IO (device U)
  deviceDescription :: device a -> String
  startCapture      :: device O -> (Vector Word8 -> IO ()) -> IO (device S)
  stopCapture       :: device S -> IO (device O)
