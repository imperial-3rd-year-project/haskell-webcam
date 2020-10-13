{-# LANGUAGE GADTs #-}

module DeviceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device

spec :: Spec
spec = do 
  describe "deviceDescription" $ do
    it "returns the first constructor argument for the unopened device" $ property $
       \x -> deviceDescription (Unopened x) == (x :: String)

    it "returns the second constructor argument for the opened device" $ property $
       \x y -> deviceDescription (Opened x y) == (y :: String)

    it "returns the second constructor argument for the streaming device" $ property $
       \x y -> deviceDescription (Streaming x y) == (y :: String)
