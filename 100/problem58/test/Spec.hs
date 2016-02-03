import Euler
import Test.Hspec

import Data.Ratio

main :: IO ()
main = hspec $ do
  describe "cornerNumbers" $
    it "returns a list of corner numbers" $ do
      cornerNumbers 1 `shouldBe` [1]
      cornerNumbers 2 `shouldBe` [3, 5, 7, 9]
      cornerNumbers 3 `shouldBe` [13, 17, 21, 25]
      cornerNumbers 4 `shouldBe` [31, 37, 43, 49]

  describe "numCornerPrimes" $
    it "returns a number of primes in a corner"$ do
      numCornerPrimes 1 `shouldBe` 0
      numCornerPrimes 2 `shouldBe` 3
      numCornerPrimes 3 `shouldBe` 2
      numCornerPrimes 4 `shouldBe` 3

  describe "spiralPrimeRatio" $
    it "returns spiral prime ratio for n layers" $ do
      spiralPrimeRatio 2 `shouldBe` 3 % 5
      spiralPrimeRatio 3 `shouldBe` 5 % 9
      spiralPrimeRatio 4 `shouldBe` 8 % 13
