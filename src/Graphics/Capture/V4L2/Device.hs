{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Graphics.Capture.V4L2.Device (Device (..)) where

import Bindings.LibV4L2 (c'v4l2_open, c'v4l2_close)
import Bindings.Posix.Fcntl (c'O_RDWR, c'O_NONBLOCK)
import Control.Monad (filterM)
import Data.Bits ((.|.))
import Data.List (isPrefixOf)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.String (withCString)
import Graphics.Capture.Class
import System.Directory (listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import System.Posix.Types (Fd)

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: Fd -> FilePath -> Device O
  Streaming :: Fd -> FilePath -> Device S

deriving instance Show (Device a)

instance VideoCapture Device where
  deviceDescription (Unopened path)    = path
  deviceDescription (Opened _ path)    = path
  deviceDescription (Streaming _ path) = path
    
  -- finds all /dev/video* which are not links and wraps them in Device type
  getDevices = do
    devs <- listDirectory deviceDir

    let relNameDevs  = filter (isPrefixOf videoDevPrefix) devs
        fullNameDevs = map (deviceDir </>) relNameDevs

    videoDevs <- filterM (fmap not . pathIsSymbolicLink) fullNameDevs
    return $ map Unopened videoDevs
    where
      deviceDir       = "/dev"
      videoDevPrefix  = "video"

  openDevice (Unopened path) = withCString path $ \p -> do
    fd <- throwErrnoIfMinus1 errorString (c'v4l2_open p (c'O_RDWR .|. c'O_NONBLOCK) 0)
    return $ Opened (fromIntegral fd) path
    where  
      errorString = "Graphics.Capture.V4L2.Device.openDevice"

  closeDevice (Opened fd path) = 
   throwErrnoIfMinus1 errorString (c'v4l2_close (fromIntegral fd)) >>
   (return $ Unopened path)
     where
       errorString = "Graphics.Capture.V4L2.Device.closeDevice"
