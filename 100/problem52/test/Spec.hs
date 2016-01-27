import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "includesSameDigits" $
    it "tells if two integers are including same digits" $ do
      includesSameDigits 1234 4231 `shouldBe` True
      includesSameDigits 1234 42311 `shouldBe` False
      includesSameDigits 41123 42311 `shouldBe` True

  describe "permutedMultiples" $
    it "tells if an integer can generate n permuted multiples" $
      permutedMultiples 2 125874 `shouldBe` True
