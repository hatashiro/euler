import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPalindrome" $
    it "tells if digits in an array forms a palindrome" $ do
      isPalindrome [1, 2, 3] `shouldBe` False
      isPalindrome [1, 2, 1] `shouldBe` True
      isPalindrome [1, 2, 5, 2, 1] `shouldBe` True

  describe "isDoubleBasePalindrome" $
    it "teels if an integer is a palindrome in both binary and decimal" $ do
      isDoubleBasePalindrome 585 `shouldBe` True
      isDoubleBasePalindrome 121 `shouldBe` False
