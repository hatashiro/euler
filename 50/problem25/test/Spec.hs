import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fibo" $ do
    it "calculates a finonacci number" $ do
      (fibo 1) `shouldBe` 1
      (fibo 2) `shouldBe` 1
      (fibo 3) `shouldBe` 2
      (fibo 4) `shouldBe` 3
      (fibo 5) `shouldBe` 5
      (fibo 6) `shouldBe` 8
      (fibo 7) `shouldBe` 13
      (fibo 8) `shouldBe` 21
      (fibo 9) `shouldBe` 34
      (fibo 10) `shouldBe` 55
      (fibo 11) `shouldBe` 89
      (fibo 12) `shouldBe` 144

  describe "memoFibo" $ do
    it "is memoized fibo" $ do
      (memoFibo 1) `shouldBe` 1
      (memoFibo 2) `shouldBe` 1
      (memoFibo 3) `shouldBe` 2
      (memoFibo 4) `shouldBe` 3
      (memoFibo 5) `shouldBe` 5
      (memoFibo 6) `shouldBe` 8
      (memoFibo 7) `shouldBe` 13
      (memoFibo 8) `shouldBe` 21
      (memoFibo 9) `shouldBe` 34
      (memoFibo 10) `shouldBe` 55
      (memoFibo 11) `shouldBe` 89
      (memoFibo 12) `shouldBe` 144

  describe "digit" $ do
    it "gets the number of digits from an integer" $ do
      (digit 3) `shouldBe` 1
      (digit 10) `shouldBe` 2
      (digit 67) `shouldBe` 2
      (digit 100) `shouldBe` 3
      (digit 1000) `shouldBe` 4
      (digit 1591) `shouldBe` 4

  describe "firstNDigitFibo" $ do
    it "gets a fibo seq having n digit for the 1st time" $ do
      (firstNDigitFibo 2) `shouldBe` 7
      (firstNDigitFibo 3) `shouldBe` 12
