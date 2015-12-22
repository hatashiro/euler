import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getFractionPart" $ do
    it "calculates the fraction part of 1 / n" $ do
      (getFractionPart 2) `shouldBe` [5]
      (getFractionPart 4) `shouldBe` [2, 5]
      (getFractionPart 5) `shouldBe` [2]
      (getFractionPart 8) `shouldBe` [1, 2, 5]
      (getFractionPart 10) `shouldBe` [1]
      (getFractionPart 100) `shouldBe` [0, 1]

    it "can calculates the infinite fraction part" $ do
      (take 3 $ getFractionPart 3) `shouldBe` [3, 3, 3]
      (take 4 $ getFractionPart 6) `shouldBe` [1, 6, 6, 6]
      (take 12 $ getFractionPart 7) `shouldBe` [1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7]
      (take 3 $ getFractionPart 9) `shouldBe` [1, 1, 1]
      (take 6 $ getFractionPart 999) `shouldBe` [0, 0, 1, 0, 0, 1]

  describe "getRecurringCycle" $ do
    it "calculates the recurring cycle of 1 / n" $ do
      (getRecurringCycle 3) `shouldBe` [3]
      (getRecurringCycle 6) `shouldBe` [6]
      (getRecurringCycle 7) `shouldBe` [1, 4, 2, 8, 5, 7]
      (getRecurringCycle 9) `shouldBe` [1]
      (getRecurringCycle 999) `shouldBe` [0, 0, 1]
