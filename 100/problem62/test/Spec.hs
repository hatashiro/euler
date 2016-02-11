import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getHashKey" $
    it "returns a hash key for a cubic integer" $
      getHashKey 41063625 `shouldBe` "01234566"
  describe "cubicPermutations" $
    it "returns the smallest cube has n cubic permutations" $
      cubicPermutations 3 `shouldBe` 41063625
