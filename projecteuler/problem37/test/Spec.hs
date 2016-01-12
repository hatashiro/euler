import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "leftTruncatablePrimesWithDigit" $
    it "returns a list of left-truncatable primes in n digits" $ do
      leftTruncatablePrimesWithDigit 1 `shouldMatchList` [2, 3, 5, 7]
      leftTruncatablePrimesWithDigit 3 `shouldContain` [797]
      leftTruncatablePrimesWithDigit 4 `shouldContain` [3797]

  describe "rightTruncatablePrimesWithDigit" $
    it "returns a list of left-truncatable primes in n digits" $ do
      rightTruncatablePrimesWithDigit 1 `shouldMatchList` [2, 3, 5, 7]
      rightTruncatablePrimesWithDigit 3 `shouldContain` [797]
      rightTruncatablePrimesWithDigit 4 `shouldContain` [3797]

  describe "truncatablePrimes" $
    it "returns a list of truncatable primes" $
      truncatablePrimes `shouldContain` [3797]
