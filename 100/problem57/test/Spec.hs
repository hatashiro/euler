import Data.Ratio
import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "sqrtIteration" $
    it "returns a result of i iteration of sqrt convergent" $ do
      sqrtIteration 1 `shouldBe` 3 % 2
      sqrtIteration 2 `shouldBe` 7 % 5
      sqrtIteration 3 `shouldBe` 17 % 12
      sqrtIteration 4 `shouldBe` 41 % 29

  describe "hasLongerNumerator" $
    it "tells if a fraction has longer numerator than denominator" $ do
      hasLongerNumerator (123 % 123) `shouldBe` False
      hasLongerNumerator (1234 % 123) `shouldBe` True
      hasLongerNumerator (1234 % 12345) `shouldBe` False
