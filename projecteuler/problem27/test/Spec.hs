import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "quadratic" $ do
    it "calculate quadratic equation" $ do
      (quadratic 10 100 1) `shouldBe` 111
      (quadratic 20 100 1) `shouldBe` 121
      (quadratic 20 100 4) `shouldBe` 196
      (quadratic (-4) 50 5) `shouldBe` 55
      ((quadratic (-4) 50) 5) `shouldBe` 55

  describe "isPrime" $ do
    it "tells if an integer is a prime" $ do
      (isPrime 2) `shouldBe` True
      (isPrime 3) `shouldBe` True
      (isPrime 4) `shouldBe` False
      (isPrime 7) `shouldBe` True
      (isPrime 8) `shouldBe` False
      (isPrime 13) `shouldBe` True
      (isPrime 15) `shouldBe` False
      (isPrime 821) `shouldBe` True
      (isPrime 833) `shouldBe` False
      (isPrime 341) `shouldBe` False

  describe "primeSeq" $ do
    it "counts the prime sequence of a given quadratic" $ do
      (primeSeq $ quadratic 1 41) `shouldBe` 40
      (primeSeq $ quadratic (-79) 1601) `shouldBe` 80
