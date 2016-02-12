import Euler
import Test.Hspec

main :: IO ()
main = hspec $
  describe "nDigitNthPowers" $
    it "returns a list of nth powers with n digits" $ do
      nDigitNthPowers 1 `shouldBe` [1..9]
      nDigitNthPowers 2 `shouldBe` [16, 25, 36, 49, 64, 81]
