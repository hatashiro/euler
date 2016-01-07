import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "digits" $
    it "returns digits of a number" $ do
      digits 13 `shouldBe` [1, 3]
      digits 1513 `shouldBe` [1, 5, 1, 3]

  describe "fromDigits" $
    it "returns an integer made by digits" $ do
      fromDigits [1, 2, 3] `shouldBe` 123
      fromDigits [2, 0] `shouldBe` 20

  describe "cancelDigit" $
    it "cancels a digit from an integer" $ do
      cancelDigit 10 0 `shouldBe` 1
      cancelDigit 243 4 `shouldBe` 23

  describe "commonDigits" $
    it "retrives common digits between two integers" $ do
      commonDigits 10 20 `shouldBe` [0]
      commonDigits 123 13 `shouldBe` [1, 3]
      commonDigits 123 45 `shouldBe` []

  describe "minimise" $
    it "minimises a fraction" $ do
      minimise (Fraction 2 4) `shouldBe` Fraction 1 2
      minimise (Fraction 21 24) `shouldBe` Fraction 7 8
      minimise (Fraction 3 7) `shouldBe` Fraction 3 7

  describe "isNontriviallyCurious" $
    it "tells if an integer is curious" $ do
      isNontriviallyCurious (Fraction 49 98) `shouldBe` True
      isNontriviallyCurious (Fraction 20 50) `shouldBe` False
      isNontriviallyCurious (Fraction 21 50) `shouldBe` False
      isNontriviallyCurious (Fraction 49 99) `shouldBe` False
