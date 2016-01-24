import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "consecutivePrimeSum" $
    it "returns a consecutive prime sum" $ do
      consecutivePrimeSum 0 6 `shouldBe` 41
      consecutivePrimeSum 3 21 `shouldBe` 953

  describe "longestConsecutivePrimeSum" $
    it "returns a longest consecutive prime sum less than n" $ do
      longestConsecutivePrimeSum 100 `shouldBe` (6, 41)
      longestConsecutivePrimeSum 1000 `shouldBe` (21, 953)
