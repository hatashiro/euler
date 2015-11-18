import Test.Hspec

import Euler

main :: IO ()
main = hspec $ do
  describe "factorial" $ do
    it "calculates factorial" $ do
      (factorial 10) `shouldBe` 3628800
      (factorial 100) `shouldBe` 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

  describe "digitSum" $ do
    it "sums up all the digits" $ do
      (digitSum 10) `shouldBe` 1
      (digitSum 123) `shouldBe` 6
      (digitSum 4000000000000) `shouldBe` 4
