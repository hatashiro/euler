import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pentagonNumber" $
    it "returns a pentagon number for an index" $
      map pentagonNumber [1..10] `shouldBe` [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

  describe "isPentagonNumber" $
    it "tells if a number is a pentagon number" $ do
      isPentagonNumber 51 `shouldBe` True
      isPentagonNumber 50 `shouldBe` False
      isPentagonNumber 1 `shouldBe` True
      isPentagonNumber 35 `shouldBe` True
      isPentagonNumber 37 `shouldBe` False

  describe "isSumPentagon" $
    it "pairs forming a pentagon number by sum of two pentagon numbers" $
      isSumPentagon (4, 7) `shouldBe` True

  describe "sumPentagonPairs" $
    it "returns a list of sum pentagon pairs" $
      sumPentagonPairs `shouldContain` [(7, 4)]
