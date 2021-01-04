{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.Display.V4L2.Device
    ( DeviceOutput
    , newV4L2Output
    ) where

import Bindings.Linux.VideoDev2
import Bindings.Posix.Fcntl (c'O_RDWR)
import Data.Vector.Storable (unsafeToForeignPtr0)
import Graphics.Display.Class 
import Graphics.Utils.ConversionUtils (centredOffset, resize)
import Graphics.Utils.Types (Resolution)
import Graphics.Utils.V4L2.Device
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke, pokeByteOff, sizeOf)
import System.Posix.IO (fdWriteBuf)
import System.Posix.Types (Fd)

data DeviceOutput a where
  -- FilePath to v4l2loopback device
  Unopened  :: Resolution -> FilePath -> DeviceOutput U
  Streaming :: Resolution -> FilePath -> Fd -> DeviceOutput S

deriving instance Show (DeviceOutput a)


instance VideoOutput DeviceOutput where
  openDevice (Unopened res path) = do
    fd <- v4l2_open path c'O_RDWR errorString 
    setFormat fd
    return $ Streaming res path fd
    where 
      errorString =  "Graphics.Display.V4L2.openDevice"
      setFormat :: Fd -> IO ()
      setFormat fd = alloca $ \(fmtPtr :: Ptr C'v4l2_format) -> do
         fillBytes fmtPtr 0 (sizeOf (undefined :: C'v4l2_format))
         format <- c'v4l2_format'fmt <$> peek fmtPtr
         let (width, height) = outputResolution
         -- The dimensions are handled by image resizing, 640x480 works with v4l2loopback
         let pixelFormat = (c'v4l2_format_u'pix format) { 
             c'v4l2_pix_format'field = c'V4L2_FIELD_NONE
           , c'v4l2_pix_format'pixelformat = c'V4L2_PIX_FMT_YUV420
           , c'v4l2_pix_format'width = fromIntegral width
           , c'v4l2_pix_format'height = fromIntegral height
         }
   
         poke fmtPtr $ C'v4l2_format { 
              c'v4l2_format'type = c'V4L2_BUF_TYPE_VIDEO_OUTPUT 
            , c'v4l2_format'fmt  = format { c'v4l2_format_u'pix = pixelFormat }
         }
   
         pokeByteOff fmtPtr 8 pixelFormat
   
         let errString = "Graphics.Capture.V4L2.DeviceOutput.open: S_FMT"
         v4l2_ioctl fd c'VIDIOC_S_FMT fmtPtr errString
         return ()
           
  writeFrame (Streaming res _ fd) img = do
    let    offset      = centredOffset res outputResolution
    let    img'        = resize offset res outputResolution img
    let    (fptr, len) = unsafeToForeignPtr0 img'
    withForeignPtr fptr $ \ptr -> do
      _ <- fdWriteBuf fd ptr (fromIntegral len)
      return ()
   
  -- TODO implement closeDevice
  closeDevice = undefined

newV4L2Output :: (Int, Int) -> String -> DeviceOutput U
newV4L2Output = Unopened

outputResolution :: (Int, Int)
outputResolution = (640, 480)
