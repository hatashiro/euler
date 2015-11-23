import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseNames" $ do
    it "parses name list string" $ do
      (parseNames "\"MARY\",\"PATRICIA\",\"LINDA\"") `shouldMatchList` ["MARY", "PATRICIA", "LINDA"]

  describe "names" $ do
    it "returns names" $ do
      names `shouldContain` ["MARY"]
      names `shouldContain` ["BARBARA"]
      names `shouldContain` ["SUSAN"]
      names `shouldContain` ["LISA"]
      names `shouldContain` ["NANCY"]
      (length names) `shouldBe` 5000
