import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "removeDuplication" $ do
    it "removes duplicated elements from an array" $ do
      (removeDuplication [1, 2, 1, 3, 4]) `shouldBe` [1, 2, 3, 4]
      (removeDuplication [1, 3, 4]) `shouldBe` [1, 3, 4]
