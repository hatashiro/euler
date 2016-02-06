import Euler
import Test.Hspec

import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "isPrimePair" $
    it "tells if two primes form a prime pair" $ do
      isPrimePair 3 5 `shouldBe` False
      isPrimePair 3 7 `shouldBe` True
      isPrimePair 109 673 `shouldBe` True

  describe "retrievePrimePairs" $
    it "retrieves prime pairs from a prime" $ do
      retrievePrimePairs 7109 `shouldBe` [(7, 109)]
      retrievePrimePairs 1097 `shouldBe` [(109, 7)]

  describe "mergePrimeSet" $
    it "merges prime pairs if a new prime forms prime pairs for others" $ do
      mergePrimeSet (Set.fromDistinctAscList [3, 7, 109]) (3, 673) `shouldBe` Set.fromList [3, 7, 109, 673]
      mergePrimeSet (Set.fromDistinctAscList [3, 7, 109]) (673, 3) `shouldBe` Set.fromList [3, 7, 109, 673]
      mergePrimeSet (Set.fromDistinctAscList [3, 7, 109]) (3, 5) `shouldBe` Set.fromList [3, 7, 109]
      mergePrimeSet (Set.fromDistinctAscList [3, 7, 109]) (11, 13) `shouldBe` Set.fromList [3, 7, 109]

  describe "primePairSet" $
    it "returns a prime pair set with n elements" $
      primePairSet 4 `shouldBe` Set.fromList [3, 7, 109, 673]
