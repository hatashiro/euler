import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "factorize" $
    it "factorizes an integer and returns the factors" $ do
      factorize 14 `shouldMatchList` [2, 7]
      factorize 15 `shouldMatchList` [3, 5]
      factorize 644 `shouldMatchList` [2, 7, 23]
      factorize 645 `shouldMatchList` [3, 5, 43]
      factorize 646 `shouldMatchList` [2, 17, 19]

  describe "nDistinctPrimeFactors" $
    it "tells if there is n distinct prime factors from an index" $ do
      nDistinctPrimeFactors 2 14 `shouldBe` True
      nDistinctPrimeFactors 3 644 `shouldBe` True
