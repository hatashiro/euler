import Euler
import Test.Hspec

numbers :: (Int -> Bool) -> [Int]
numbers f = filter f [1..]
main :: IO ()
main = hspec $ do
  describe "isTriangle" $
    it "tells if a number is triangle" $
      take 5 (numbers isTriangle) `shouldBe` [1, 3, 6, 10, 15]

  describe "isSquare" $
    it "tells if a number is square" $
      take 5 (numbers isSquare) `shouldBe` [1, 4, 9, 16, 25]

  describe "isPentagonal" $
    it "tells if a number is pentagonal" $
      take 5 (numbers isPentagonal) `shouldBe` [1, 5, 12, 22, 35]

  describe "isHexagonal" $
    it "tells if a number is hexagonal" $
      take 5 (numbers isHexagonal) `shouldBe` [1, 6, 15, 28, 45]

  describe "isHeptagonal" $
    it "tells if a number is heptagonal" $
      take 5 (numbers isHeptagonal) `shouldBe` [1, 7, 18, 34, 55]

  describe "isOctagonal" $
    it "tells if a number is octagonal" $
      take 5 (numbers isOctagonal) `shouldBe` [1, 8, 21, 40, 65]

  describe "cyclicalFigurate3" $
    it "returns a list of 3 cyclical figurate numbers" $
      cyclicalFigurate3 [isTriangle, isSquare, isPentagonal] `shouldContain` [(8128, 2882, 8281)]
