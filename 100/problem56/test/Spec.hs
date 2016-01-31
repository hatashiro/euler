import Euler
import Test.Hspec

main :: IO ()
main = hspec $
  describe "digitSum" $
    it "calculates sum of digits" $ do
      digitSum (100^100) `shouldBe` 1
      digitSum 1234 `shouldBe` 10
