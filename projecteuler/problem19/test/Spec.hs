import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getDayOfWeek" $ do
    it "gets a right day of week" $ do
      (getDayOfWeek 1900 1 1) `shouldBe` 1
      (getDayOfWeek 2012 6 6) `shouldBe` 3
      (getDayOfWeek 2015 11 17) `shouldBe` 2

  describe "isLeapYear" $ do
    it "tells if a year ia a leap year" $ do
      (isLeapYear 1904) `shouldBe` True
      (isLeapYear 1900) `shouldBe` False
      (isLeapYear 2000) `shouldBe` True
      (isLeapYear 2016) `shouldBe` True
      (isLeapYear 2015) `shouldBe` False

  describe "daysOfMonth" $ do
    it "returns days of a month" $ do
      (daysOfMonth 2015 1) `shouldBe` 31
      (daysOfMonth 2016 2) `shouldBe` 29
      (daysOfMonth 2015 2) `shouldBe` 28
      (daysOfMonth 2015 3) `shouldBe` 31
      (daysOfMonth 2015 4) `shouldBe` 30
      (daysOfMonth 2015 5) `shouldBe` 31
      (daysOfMonth 2015 6) `shouldBe` 30
      (daysOfMonth 2015 7) `shouldBe` 31
      (daysOfMonth 2015 8) `shouldBe` 31
      (daysOfMonth 2015 9) `shouldBe` 30
      (daysOfMonth 2015 10) `shouldBe` 31
      (daysOfMonth 2015 11) `shouldBe` 30
      (daysOfMonth 2015 12) `shouldBe` 31

  describe "daysOfYear" $ do
    it "returns days of an year" $ do
      (daysOfYear 1904) `shouldBe` 366
      (daysOfYear 1900) `shouldBe` 365
      (daysOfYear 2000) `shouldBe` 366
      (daysOfYear 2016) `shouldBe` 366
      (daysOfYear 2015) `shouldBe` 365
