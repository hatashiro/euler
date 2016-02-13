import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sqrtFracPair" $
    it "returns sqrt fraction pair" $ do
      sqrtFracPair 23 0 `shouldBe` (4, 1)
      sqrtFracPair 23 1 `shouldBe` (3, 7)
      sqrtFracPair 23 2 `shouldBe` (3, 2)
      sqrtFracPair 23 3 `shouldBe` (4, 7)
      sqrtFracPair 23 4 `shouldBe` (4, 1)

  describe "sqrtFracPeriod" $
    it "returns sqrt frac period" $ do
      sqrtFracPeriod 2 `shouldBe` 1
      sqrtFracPeriod 3 `shouldBe` 2
      sqrtFracPeriod 4 `shouldBe` 0
      sqrtFracPeriod 5 `shouldBe` 1
      sqrtFracPeriod 11 `shouldBe` 2
      sqrtFracPeriod 12 `shouldBe` 2
      sqrtFracPeriod 13 `shouldBe` 5
