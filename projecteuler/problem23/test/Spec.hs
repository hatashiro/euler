import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "properDivisors" $ do
    it "returns a list of the proper divisors for a number" $ do
      (properDivisors 12) `shouldMatchList` [1, 2, 3, 4, 6]

  describe "isAbundant" $ do
    it "tells if a number is abundant" $ do
      (isAbundant 12) `shouldBe` True

  describe "isAbundantSum" $ do
    it "tells if a number can be abundant sum" $ do
      (isAbundantSum 24) `shouldBe` True
      (isAbundantSum 25) `shouldBe` False
      (isAbundantSum 26) `shouldBe` False
      (isAbundantSum 28) `shouldBe` False
      (isAbundantSum 30) `shouldBe` True
      (isAbundantSum 36) `shouldBe` True
      (isAbundantSum 28123) `shouldBe` True
      (isAbundantSum 28124) `shouldBe` True
