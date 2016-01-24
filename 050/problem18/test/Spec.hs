import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getRow" $ do
    it "gets proper row" $ do
      (getRow 0) `shouldBe` (Just "75")
      (getRow 1) `shouldBe` (Just "95 64")
    it "gets Nothing on wrong row" $ do
      (getRow 100) `shouldBe` Nothing

  describe "getVal" $ do
    it "gets proper tree node value" $ do
      (getVal 5 2) `shouldBe` (Just 23)
      (getVal 13 13) `shouldBe` (Just 31)
      (getVal 14 13) `shouldBe` (Just 4)
      (getVal 14 14) `shouldBe` (Just 23)

    it "gets Nothing for wrong node" $ do
      (getVal 100 100) `shouldBe` Nothing

  describe "createNode" $ do
    it "returns NullNode when no node for indices" $ do
      (createNode 100 100) `shouldBe` NullNode

    it "returns correct Node" $ do
      (createNode 13 13) `shouldBe` (Node 31 (Node 4 NullNode NullNode) (Node 23 NullNode NullNode))

  describe "maxSum" $ do
    it "returns max sum" $ do
      (maxSum (Node 37 NullNode NullNode)) `shouldBe` 37
      (maxSum (Node 37 (Node 20 NullNode NullNode) (Node 30 NullNode NullNode))) `shouldBe` 67

    it "returns 0 for NullNode" $ do
      (maxSum NullNode) `shouldBe` 0
