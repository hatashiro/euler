import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "number letter count" $ do
    it "counts the total number of letters used in a written number" $ do
      (numberLetterCount 1) `shouldBe` 3
      (numberLetterCount 15) `shouldBe` (length "fifteen")
      (numberLetterCount 20) `shouldBe` (length "twenty")
      (numberLetterCount 25) `shouldBe` (length "twentyfive")
      (numberLetterCount 125) `shouldBe` (length "onehundredandtwentyfive")
      (numberLetterCount 116) `shouldBe` (length "onehundredandsixteen")
      (numberLetterCount 430) `shouldBe` (length "fourhundredandthirty")
      (numberLetterCount 1000) `shouldBe` (length "onethousand")
