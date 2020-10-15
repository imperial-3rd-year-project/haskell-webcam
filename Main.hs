module Main where

import Control.Monad (forever)
import Control.Concurrent (yield)
import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device

main :: IO ()
main = do
  devs <- getDevices :: IO [Device U]
  putStrLn $ show devs
  putStrLn $ deviceDescription (Unopened "/dev/video0")
  let device = Unopened "/dev/video0"
  opened <- openDevice device
  startCapture opened (const $ putStrLn "Frame!")
  putStrLn "Hello, Haskell!"
  forever yield
