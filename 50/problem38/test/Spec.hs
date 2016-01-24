import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pandigitalNumbers" $
    it "returns a list of pandigital numbers generated with digits, in descending order" $ do
      pandigitalNumbers [1] `shouldBe` [1]
      pandigitalNumbers [1..2] `shouldBe` reverse [12, 21]
      pandigitalNumbers [1..3] `shouldBe` reverse [123, 132, 213, 231, 312, 321]


  describe "isConcatenatedMultiple" $
    it "tells if an integer can be expressed as a concatenated multiple" $ do
      isConcatenatedMultiple 192384576 `shouldBe` True
      isConcatenatedMultiple 918273645 `shouldBe` True
      isConcatenatedMultiple 1235418   `shouldBe` False

  describe "canFormConcatenatedMultiple" $
    it "tells if a multiplier can form an integer with concatenated multiple" $ do
      canFormConcatenatedMultiple 192 [1, 9, 2, 3, 8, 4, 5, 7, 6] `shouldBe` True
      canFormConcatenatedMultiple 9 [9, 1, 8, 2, 7, 3, 6, 4, 5] `shouldBe` True
      canFormConcatenatedMultiple 8 [9, 1, 8, 2, 7, 3, 6, 4, 5] `shouldBe` False
      canFormConcatenatedMultiple 91 [9, 1, 8, 2, 7, 3, 6, 4, 5] `shouldBe` False
      canFormConcatenatedMultiple 918 [9, 1, 8, 2, 7, 3, 6, 4, 5] `shouldBe` False
