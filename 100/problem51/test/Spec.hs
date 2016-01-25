import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "findDigitPositions" $
    it "finds digit positions of a digit from an integer" $ do
      findDigitPositions 15134 1 `shouldBe` [0, 2]
      findDigitPositions 15134 5 `shouldBe` [1]
      findDigitPositions 15134 0 `shouldBe` []

  describe "replaceDigitPositions" $
    it "replaces digits in the positions with another digit" $ do
      replaceDigitPositions 15134 [0, 2] 3 `shouldBe` 35334
      replaceDigitPositions 15134 [1] 9 `shouldBe` 19134

  describe "nPrimeFamily" $
    it "returns a smallest value of n prime value family" $ do
      nPrimeFamily 6 `shouldBe` Just 13
      nPrimeFamily 7 `shouldBe` Just 56003
