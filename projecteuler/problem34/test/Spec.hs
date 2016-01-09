import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "factorial" $
    it "calculates factorial of an integer" $ do
      factorial 1 `shouldBe` 1
      factorial 2 `shouldBe` 2
      factorial 3 `shouldBe` 6
      factorial 4 `shouldBe` 24

  describe "digits" $
    it "returns digits of an integer" $ do
      digits 136 `shouldMatchList` [1, 3, 6]
      digits 1601 `shouldMatchList` [1, 6, 0, 1]
