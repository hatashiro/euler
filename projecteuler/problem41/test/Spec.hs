import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ndigitPandigitals" $
    it "returns a list of pandigital numbers with n digit(s)" $ do
      ndigitPandigitals 1 `shouldMatchList` [1]
      ndigitPandigitals 2 `shouldMatchList` [12, 21]
      ndigitPandigitals 3 `shouldMatchList` [123, 132,
                                             213, 231,
                                             312, 321]

  describe "pandigitals" $
    it "returns a list of all pandigial numbers" $ do
      pandigitals `shouldContain` ndigitPandigitals 1
      pandigitals `shouldContain` ndigitPandigitals 2
      pandigitals `shouldContain` ndigitPandigitals 3
      pandigitals `shouldContain` ndigitPandigitals 4
      pandigitals `shouldContain` ndigitPandigitals 5
      pandigitals `shouldContain` ndigitPandigitals 6
      pandigitals `shouldContain` ndigitPandigitals 7
      pandigitals `shouldContain` ndigitPandigitals 8
      pandigitals `shouldContain` ndigitPandigitals 9

  describe "isPrime" $
    it "tells if an integer is prime" $ do
     isPrime 2 `shouldBe` True
     isPrime 3 `shouldBe` True
     isPrime 4 `shouldBe` False
     isPrime 5 `shouldBe` True
     isPrime 6 `shouldBe` False
     isPrime 7 `shouldBe` True
     isPrime 13 `shouldBe` True
     isPrime 14 `shouldBe` False
