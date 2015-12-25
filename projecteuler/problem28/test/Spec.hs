import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getLayerCellCount" $ do
    it "gets the number of cells in a layer" $ do
      (getLayerCellCount 1) `shouldBe` 1
      (getLayerCellCount 2) `shouldBe` 8
      (getLayerCellCount 3) `shouldBe` 16


  describe "getFirstNum" $ do
    it "gets the first number in a layer" $ do
      (getFirstNum 1) `shouldBe` 0
      (getFirstNum 2) `shouldBe` 1
      (getFirstNum 3) `shouldBe` 9
      (getFirstNum 4) `shouldBe` 25

  describe "getCornerSum" $ do
    it "gets a sum of numbers at corners in a layer" $ do
      (getCornerSum 1) `shouldBe` 1
      (getCornerSum 2) `shouldBe` 24
      (getCornerSum 3) `shouldBe` 76

  describe "getDiagnoalSum" $ do
    it "calculates a diagonal sum of a n x n square" $ do
      (getDiagnoalSum 5) `shouldBe` 101
