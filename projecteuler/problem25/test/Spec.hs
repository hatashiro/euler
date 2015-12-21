import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "" $ do
    it "" $ do
      1 `shouldBe` 1
