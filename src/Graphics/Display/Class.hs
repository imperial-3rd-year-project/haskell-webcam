module Graphics.Display.Class (VideoOutput (..), U, S) where

import Data.Vector.Storable
import Data.Word
import Graphics.Utils.Types (U, S)

class VideoOutput device where
  openDevice        :: device U -> IO (device S)
  closeDevice       :: device S -> IO (device U)
  writeFrame        :: device S -> Vector Word8 -> IO ()
