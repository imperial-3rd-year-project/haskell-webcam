{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Graphics.Capture.V4L2.Device (Device (..)) where

import Control.Monad (filterM)
import Data.List (isPrefixOf)
import Graphics.Capture.Class
import System.Directory (listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import System.Posix.Internals (FD)

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: FD -> FilePath -> Device O
  Streaming :: FD -> FilePath -> Device S

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
