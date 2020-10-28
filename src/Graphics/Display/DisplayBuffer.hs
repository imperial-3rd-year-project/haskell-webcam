{-# LANGUAGE GADTs #-}

module Graphics.Display.DisplayBuffer 
  ( DisplayBuffer
  , newBuffer
  ) where

import Control.Concurrent (forkFinally, ThreadId, killThread)
import Control.Concurrent.STM.TBQueue
import Control.Monad (void, forever)
import Control.Monad.STM

import qualified Data.Vector.Storable as V
import Data.Word (Word8)

import Graphics.Display.Class

import Numeric.Natural

data DisplayBuffer device a where
  Unopened  :: Natural -> device U -> DisplayBuffer device U
  Streaming :: Natural -> device S -> TBQueue (V.Vector Word8) -> ThreadId -> DisplayBuffer device S

instance VideoOutput device => VideoOutput (DisplayBuffer device) where
  openDevice (Unopened size dev) = do
    queue <- newTBQueueIO size

    opened <- openDevice dev

    writeThread <- forkFinally (queue `writeTo` opened) (\_ -> flushQueue queue)

    return $ Streaming size opened queue writeThread

    where
      writeTo :: VideoOutput device => TBQueue (V.Vector Word8) -> device S -> IO ()
      queue `writeTo` dst = forever $ atomically (readTBQueue queue) >>= writeFrame dst

      flushQueue :: TBQueue a -> IO ()
      flushQueue queue = atomically . void . flushTBQueue $ queue

  writeFrame (Streaming _ _ queue _) = atomically . writeTBQueue queue

  closeDevice (Streaming size dev _ writeThread) = do
    killThread writeThread
    closed <- closeDevice dev
    return $ Unopened size closed

newBuffer :: VideoOutput device => Natural -> device U -> DisplayBuffer device U
newBuffer = Unopened
