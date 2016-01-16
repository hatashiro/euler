import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "isPythagoreanTriples" $
    it "tells if three integers form a pythagorian triple" $ do
      isPythagoreanTriples 1 2 3 `shouldBe` False
      isPythagoreanTriples 3 4 5 `shouldBe` True
      isPythagoreanTriples 20 48 52 `shouldBe` True
      isPythagoreanTriples 24 45 51 `shouldBe` True
      isPythagoreanTriples 30 40 50 `shouldBe` True

  describe "pythagorianTriples" $
    it "returns a list of pythagorian triples a sum of which is an integer" $
      pythagorianTriples 120 `shouldMatchList` [(20,48,52), (24,45,51), (30,40,50)]
