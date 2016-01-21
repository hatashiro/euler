import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPrime" $
    it "tells if an integer is prime" $ do
      isPrime 3 `shouldBe` True
      isPrime 4 `shouldBe` False
      isPrime 5 `shouldBe` True
      isPrime 6 `shouldBe` False
      isPrime 13 `shouldBe` True
      isPrime 15 `shouldBe` False

  describe "areInTheSameInterval" $
    it "tells if 3 integers are in the same interval" $ do
      areInTheSameInterval 1 3 5 `shouldBe` True
      areInTheSameInterval 1487 4817 8147 `shouldBe` True
      areInTheSameInterval 12 24 35 `shouldBe` False

  describe "findPrimePermutation" $
    it "finds prime permutations from digits" $
      findPrimePermutation [1, 4, 7, 8] `shouldBe` [(1487, 4817, 8147)]
