import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isqrt" $
    it "returns an integer square root if exists" $ do
      isqrt 1 `shouldBe` Just 1
      isqrt 2 `shouldBe` Nothing
      isqrt 4 `shouldBe` Just 2
      isqrt 10 `shouldBe` Nothing
      isqrt 16 `shouldBe` Just 4

  describe "isTriangle" $
    it "tells if an integer is triangle" $
      filter isTriangle [1..15] `shouldBe` [1, 3, 6, 10, 15]

  describe "isPentagonal" $
    it "tells if an integer is pentagonal" $
      filter isPentagonal [1..35] `shouldBe` [1, 5, 12, 22, 35]

  describe "isHexagonal" $
    it "tells if an integer is hexagonal" $
      filter isHexagonal [1..45] `shouldBe` [1, 6, 15, 28, 45]

  describe "hexagonals" $
    it "is a list of hexagonal numbers" $
      take 5 hexagonals `shouldBe` [1, 6, 15, 28, 45]
