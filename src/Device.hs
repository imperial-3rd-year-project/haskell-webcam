{-# LANGUAGE GADTs #-}

module Device (Device (Unopened, Opened, Streaming), deviceDescription ) where 

import System.Posix.Internals(FD)
import Types

data Device a where
  Unopened  :: FilePath       -> Device U
  Opened    :: FD -> FilePath -> Device O
  Streaming :: FD -> FilePath -> Device S




deviceDescription :: Device a -> String
deviceDescription (Unopened path)    = path
deviceDescription (Opened _ path)    = path
deviceDescription (Streaming _ path) = path

