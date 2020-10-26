module Graphics.Display.Class (VideoOutput (..)) where

import Data.Vector.Storable
import Data.Word
import Graphics.Utils.Types (U, O, S, Resolution)

class VideoOutput device where
  openDevice        :: device U -> IO (device S)
  closeDevice       :: device S -> IO (device U)
  writeFrame        :: device S -> Vector Word8 -> IO ()



