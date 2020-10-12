module SampleSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "sample" $ do 
    it "simple example works" $
      head [1..3] `shouldBe` 1
