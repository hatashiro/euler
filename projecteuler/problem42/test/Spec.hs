import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseWords" $
    it "parse words from a raw word list string" $
      parseWords "\"hello\",\"world\",\"33\"" `shouldBe` ["hello", "world", "33"]

  describe "getWordValue" $
    it "returns a word value for a string" $
      getWordValue "SKY" `shouldBe` 55

  describe "triangleNumber" $
    it "returns a triangle number for an index" $
      map triangleNumber [1..10] `shouldBe` [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]

  describe "isTriangleNumber" $
    it "returns if a number is a triangle number" $ do
      isTriangleNumber 45 `shouldBe` True
      isTriangleNumber 50 `shouldBe` False
      isTriangleNumber 55 `shouldBe` True
