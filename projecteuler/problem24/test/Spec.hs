import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fac" $ do
    it "calculates factorial" $ do
      (fac 1) `shouldBe` 1
      (fac 2) `shouldBe` 2
      (fac 3) `shouldBe` 6
      (fac 10) `shouldBe` 3628800

  describe "deleteAt" $ do
    it "delete an element in an index" $ do
      (deleteAt 0 [1, 2, 3]) `shouldBe` [2, 3]
      (deleteAt 1 [1, 2, 3]) `shouldBe` [1, 3]
      (deleteAt 2 [1, 2, 3]) `shouldBe` [1, 2]

  describe "nthPermutation" $ do
    it "calculates nth permutation" $ do
      (nthPermutation [0, 1, 2] 0) `shouldBe` [0, 1, 2]
      (nthPermutation [0, 1, 2] 1) `shouldBe` [0, 2, 1]
      (nthPermutation [0, 1, 2] 2) `shouldBe` [1, 0, 2]
      (nthPermutation [0, 1, 2] 3) `shouldBe` [1, 2, 0]
      (nthPermutation [0, 1, 2] 4) `shouldBe` [2, 0, 1]
      (nthPermutation [0, 1, 2] 5) `shouldBe` [2, 1, 0]
