import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "number letter count" $ do
    it "counts the total number of letters used in a written number" $ do
      (numberLetterCount 1) `shouldBe` 3
