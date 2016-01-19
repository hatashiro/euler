import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pentagonNumber" $
    it "returns a pentagon number for an index" $
      map pentagonNumber [1..10] `shouldBe` [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

  describe "isqrt" $
    it "returns an integer sqrt if exists" $ do
      isqrt 1 `shouldBe` Just 1
      isqrt 2 `shouldBe` Nothing
      isqrt 4 `shouldBe` Just 2
      isqrt 15 `shouldBe` Nothing
      isqrt 16 `shouldBe` Just 4
      isqrt 23 `shouldBe` Nothing
      isqrt 25 `shouldBe` Just 5

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
