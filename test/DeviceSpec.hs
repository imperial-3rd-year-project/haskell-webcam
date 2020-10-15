{-# LANGUAGE GADTs #-}

module DeviceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Graphics.Capture.Class
import Graphics.Capture.V4L2.Device

import System.Posix.Types (Fd)

instance Arbitrary Fd where
  arbitrary = arbitrarySizedIntegral

spec :: Spec
spec = do 
  describe "deviceDescription" $ do
    it "returns the first constructor argument for the unopened device" $ property $
       \x -> deviceDescription (Unopened x) == (x :: String)

    it "returns the second constructor argument for the opened device" $ property $
        \x y -> deviceDescription (Opened x y) == (y :: String)

   -- TODO Find a way to use ThreadId with its constructor
   -- it "returns the second constructor argument for the streaming device" $ property $
   --    \x y -> deviceDescription (Streaming x y ) == (y :: String)
