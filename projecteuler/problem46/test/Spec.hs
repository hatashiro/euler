import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPrime" $
    it "tells if an integer is prime" $ do
      isPrime 2 `shouldBe` True
      isPrime 3 `shouldBe` True
      isPrime 4 `shouldBe` False
      isPrime 5 `shouldBe` True
      isPrime 6 `shouldBe` False
      isPrime 7 `shouldBe` True
      isPrime 13 `shouldBe` True
      isPrime 19 `shouldBe` True
      isPrime 20 `shouldBe` False

  describe "primes" $
    it "is a list of prime numbers" $
      take 7 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17]

  describe "primesLte" $
    it "returns a list of primes less than or equal to n" $
      primesLte 13 `shouldBe` [2, 3, 5, 7, 11, 13]

  describe "oddComps" $
    it "is a list of odd composite integers" $
      take 6 oddComps `shouldBe` [9, 15, 21, 25, 27, 33]

  describe "isqrt" $
    it "returns a integer square root if exists" $ do
      isqrt 4 `shouldBe` Just 2
      isqrt 5 `shouldBe` Nothing
      isqrt 8 `shouldBe` Nothing
      isqrt 9 `shouldBe` Just 3

  describe "goldbachConjecture" $
    it "tells if an odd composite and a prime satisfies the Goldbach's conjecture" $ do
      goldbachConjecture 9 `shouldBe` True
      goldbachConjecture 15 `shouldBe` True
      goldbachConjecture 21 `shouldBe` True
      goldbachConjecture 25 `shouldBe` True
      goldbachConjecture 27 `shouldBe` True
      goldbachConjecture 33 `shouldBe` True
