import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fac" $
    it "calculates a factorial" $ do
      fac 3 `shouldBe` 6
      fac 5 `shouldBe` 120

  describe "combination" $
    it "calculates a combination" $ do
      combination 5 2 `shouldBe` 10
      combination 23 10 `shouldBe` 1144066
      combination 23 13 `shouldBe` 1144066
