import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPrime" $
    it "tells if an integer is prime" $ do
      isPrime 3 `shouldBe` True
      isPrime 2 `shouldBe` True
      isPrime 4 `shouldBe` False
      isPrime 7 `shouldBe` True
      isPrime 197 `shouldBe` True
      isPrime 971 `shouldBe` True
      isPrime 719 `shouldBe` True
      isPrime 154 `shouldBe` False

  describe "getCircularNumbers" $
    it "returns a list of circular numbers for an integer" $ do
      getCircularNumbers 197 `shouldMatchList` [197, 971, 719]
      getCircularNumbers 13 `shouldMatchList` [13, 31]
      getCircularNumbers 3 `shouldMatchList` [3]
