import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPalindrome" $
    it "tells if an integer is a palindrome" $ do
      isPalindrome 1001 `shouldBe` True
      isPalindrome 12321 `shouldBe` True
      isPalindrome 151233 `shouldBe` False
      isPalindrome 12331 `shouldBe` False

  describe "reverseAdd" $
    it "reverses and adds an integer to itself" $ do
      reverseAdd 349 `shouldBe` 1292
      reverseAdd 1292 `shouldBe` 4213

  describe "itersForPalindrome" $
    it "returns the number of iterations needed to form a palindrome" $ do
      itersForPalindrome 47 `shouldBe` Just 1
      itersForPalindrome 349 `shouldBe` Just 3
      itersForPalindrome 196 `shouldBe` Nothing
