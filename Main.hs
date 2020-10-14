module Main where

import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device

main :: IO ()
main = do
  devs <- getDevices :: IO [Device U]
  putStrLn $ show devs
  putStrLn $ deviceDescription (Unopened "/dev/video0")
  putStrLn "Hello, Haskell!"