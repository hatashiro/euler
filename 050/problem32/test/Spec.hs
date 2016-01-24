import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "consistOf" $
    it "telles if an integer consists of given integers" $ do
      consistOf 1478 [1, 4, 7, 8] `shouldBe` True
      consistOf 1478 [1, 4, 7, 9] `shouldBe` False
      consistOf 123 [1, 2, 3] `shouldBe` True
      consistOf 12345679 [1, 2, 3, 4, 5, 6, 7, 9] `shouldBe` True

  describe "canDigitMul" $
    it "tells if x * y can be z with digits for each terms" $ do
      canDigitMul 1 2 2 `shouldBe` True
      canDigitMul 2 3 4 `shouldBe` True
      canDigitMul 2 3 6 `shouldBe` False

  describe "multipliableDigitsInSum" $
    it "returns a list of possible digit pairs" $ do
      multipliableDigitsInSum 3 `shouldBe` [(1, 1)]
      multipliableDigitsInSum 10 `shouldBe` [(1,4),(2,3)]

  describe "integersFromListWithDigit" $
    it "returns a list of possible integers from a list with a digit" $ do
      integersFromListWithDigit 2 [1, 2, 3] `shouldMatchList` [12, 13, 23, 21, 31, 32]
      integersFromListWithDigit 1 [1, 2] `shouldMatchList` [1, 2]

  describe "diff" $
    it "diffs two lists" $
      diff [1, 2, 3] [1, 3] `shouldBe` [2]
