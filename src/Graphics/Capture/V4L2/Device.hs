{-# LANGUAGE GADTs #-}

module Graphics.Capture.V4L2.Device (Device (..)) where

import Graphics.Capture.Class
import System.Posix.Internals (FD)

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: FD -> FilePath -> Device O
  Streaming :: FD -> FilePath -> Device S


instance VideoCapture Device where
  deviceDescription (Unopened path)    = path
  deviceDescription (Opened _ path)    = path
  deviceDescription (Streaming _ path) = path
