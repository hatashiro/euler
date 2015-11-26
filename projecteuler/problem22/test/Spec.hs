import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseNames" $ do
    it "parses name list string" $ do
      (parseNames "\"MARY\",\"PATRICIA\",\"LINDA\"") `shouldMatchList` ["MARY", "PATRICIA", "LINDA"]

  describe "charValue" $ do
    it "converts a char into its value(order)" $ do
      (charValue 'C') `shouldBe` 3
      (charValue 'O') `shouldBe` 15
      (charValue 'L') `shouldBe` 12
      (charValue 'I') `shouldBe` 9
      (charValue 'N') `shouldBe` 14

  describe "stringValue" $ do
    it "sums its char values" $ do
      (stringValue "COLIN") `shouldBe` 53

  describe "namesScore" $ do
    it "sums its string values times order" $ do
      (namesScore ["COLIN", "COLIN"]) `shouldBe` (53*3)
      (namesScore ["COLIN", "A"]) `shouldBe` (53*2 + 1)

  describe "names" $ do
    it "returns IO names" $ do
      ns <- names
      ns `shouldContain` ["COLIN"]
