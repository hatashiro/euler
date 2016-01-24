import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pandigitals" $
    it "returns a list of 0-9 pandigital numbers" $ do
      pandigitals `shouldContain` [[1,2,3,4,5,6,7,8,9,0]]
      pandigitals `shouldContain` [[2,3,5,1,4,6,9,7,0,8]]
      pandigitals `shouldNotContain` [[0,1,2,3,4,5,6,7,8,9]]

  describe "sublist" $
    it "returns a sublist of a list" $ do
      sublist 0 2 [1, 2, 3] `shouldBe` [1, 2]
      sublist 3 3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] `shouldBe` [4, 5, 6]

  describe "isSubstringDivisible" $
    it "tells if an integer is sub-string divisible" $ do
      isSubstringDivisible [1,4,0,6,3,5,7,2,8,9] `shouldBe` True
      isSubstringDivisible [1,4,0,6,3,5,7,2,8,8] `shouldBe` False
