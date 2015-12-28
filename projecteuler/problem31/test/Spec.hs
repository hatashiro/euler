import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "coins" $
    it "is a list of possible coins" $
      coins `shouldBe` [1, 2, 5, 10, 20, 50, 100, 200]

  describe "knapsack" $
    it "calculates the number of all possible ways to solve knapsack problem" $ do
      knapsack [1, 2, 4] 1 `shouldBe` 1
      knapsack [1, 2, 4] 2 `shouldBe` 2
      knapsack [1, 2, 4] 3 `shouldBe` 2
      knapsack [1, 2, 4] 4 `shouldBe` 4
