import Euler
import Test.Hspec

main :: IO ()
main = hspec $
  describe "lastDigits" $
    it "returns a list of last digits" $ do
      lastDigits 3 1235150198 `shouldBe` [1, 9, 8]
      lastDigits 5 1235151231 `shouldBe` [5, 1, 2, 3, 1]
