import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Fraction" $ do
    it "represents a fraction" $ do
      toFloat (Fraction 7 5) `shouldBe` 1.4
      Fraction 5 1 `shouldBe` 5

    it "adds and subtracts" $ do
      Fraction 1 1 + Fraction 1 2 `shouldBe` Fraction 3 2
      3 + Fraction 1 2 `shouldBe` Fraction 7 2
      3 - Fraction 1 2 `shouldBe` Fraction 5 2
      Fraction 1 2 - Fraction 1 3 `shouldBe` Fraction 1 6

    it "multiplies and divides" $ do
      Fraction 3 2 * Fraction 4 (-5) `shouldBe` Fraction (-6) 5
      Fraction 3 2 / Fraction (-5) 4 `shouldBe` Fraction (-6) 5

  describe "sqrtIteration" $
    it "returns a result of i iteration of sqrt convergent" $ do
      sqrtIteration 1 `shouldBe` Fraction 3 2
      sqrtIteration 2 `shouldBe` Fraction 7 5
      sqrtIteration 3 `shouldBe` Fraction 17 12
      sqrtIteration 4 `shouldBe` Fraction 41 29

  describe "hasLongerNumerator" $
    it "tells if a fraction has longer numerator than denominator" $ do
      hasLongerNumerator (Fraction 123 123) `shouldBe` False
      hasLongerNumerator (Fraction 1234 123) `shouldBe` True
      hasLongerNumerator (Fraction 1234 12345) `shouldBe` False
