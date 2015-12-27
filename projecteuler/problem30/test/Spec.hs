import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "digits" $
    it "produces an array of digits from an integer" $ do
      digits 1234 `shouldBe` [1, 2, 3, 4]
      digits 151363 `shouldBe` [1, 5, 1, 3, 6, 3]

  describe "isDigitNthPower" $
    it "tells if an integer is a digit fifth power" $ do
      isDigitNthPower 4 1 `shouldBe` False
      isDigitNthPower 4 1635 `shouldBe` False
      isDigitNthPower 4 1634 `shouldBe` True
      isDigitNthPower 4 8208 `shouldBe` True
      isDigitNthPower 4 9474 `shouldBe` True
      isDigitNthPower 4 1234 `shouldBe` False
      isDigitNthPower 4 614  `shouldBe` False
