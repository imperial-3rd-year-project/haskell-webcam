module DeviceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Device

spec :: Spec
spec = do 
  describe "deviceDescription" $ do
    it "Uses the name given in contstruction" $ property $
       \x -> deviceDescription (Device x) == (x :: String)
