import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "d" $ do
    it "returns sum of proper divisors" $ do
      (d 0) `shouldBe` 0
      (d 1) `shouldBe` 0
      (d 9) `shouldBe` 4
      (d 100) `shouldBe` (sum [1, 2, 4, 5, 10, 20, 25, 50])
      (d 220) `shouldBe` 284
      (d 284) `shouldBe` 220
      (d 10) `shouldBe` (1 + 2 + 5)

  describe "isAmicable" $ do
    it "tells a number is an amicable number" $ do
      (isAmicable 1) `shouldBe` False
      (isAmicable 2) `shouldBe` False
      (isAmicable 100) `shouldBe` False
      (isAmicable 220) `shouldBe` True
      (isAmicable 284) `shouldBe` True
      (isAmicable 28) `shouldBe` False