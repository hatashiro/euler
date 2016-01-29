import Euler
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseHands" $
    it "parse two hands from a string" $
      parseHands "5H 5C 6S 7S KD 2C 3S 8S 8D TD" `shouldBe`
        ([(5, 0), (5, 2), (6, 3), (7, 3), (13, 1)],
         [(2, 0), (3, 3), (8, 1), (8, 3), (10, 1)])

  describe "flush" $
    it "tells if a hand is flush" $ do
      flush [(10, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      flush [(7, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      flush [(2, 0), (5, 0), (8, 0), (9, 0), (11, 0)] `shouldBe` Just (11, 0)
      flush [(2, 0), (5, 2), (8, 0), (9, 0), (11, 0)] `shouldBe` Nothing

  describe "straight" $
    it "tells if a hand is straight" $ do
      straight [(10, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      straight [(10, 0), (11, 3), (12, 2), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      straight [(10, 0), (11, 3), (12, 2), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      straight [(2, 0), (11, 3), (12, 2), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      straight [(2, 0), (3, 3), (12, 2), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      straight [(9, 0), (11, 3), (12, 2), (13, 0), (14, 0)] `shouldBe` Nothing

  describe "straightFlush" $
    it "tells if a hand is straight flush" $ do
      straightFlush [(3, 0), (4, 0), (5, 0), (6, 0), (7, 0)] `shouldBe` Just (7, 0)
      straightFlush [(3, 0), (4, 3), (5, 0), (6, 0), (7, 0)] `shouldBe` Nothing
      straightFlush [(3, 0), (4, 0), (5, 0), (6, 0), (8, 0)] `shouldBe` Nothing

  describe "royalFlush" $
    it "tells if a hand is royal flush" $ do
      royalFlush [(10, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Just (14, 0)
      royalFlush [(7, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Nothing
      royalFlush [(10, 0), (11, 1), (12, 0), (13, 0), (14, 0)] `shouldBe` Nothing
      royalFlush [(3, 0), (4, 0), (5, 0), (6, 0), (7, 0)] `shouldBe` Nothing

  describe "highCard" $
    it "returns a high card of a hand" $ do
      highCard [(10, 0), (11, 0), (12, 0), (13, 0), (14, 0)] `shouldBe` Just [(14, 0), (13, 0), (12, 0), (11, 0), (10, 0)]
      highCard [(3, 0), (4, 0), (5, 0), (7, 0), (7, 0)] `shouldBe` Just [(5, 0), (4, 0), (3, 0)]

  describe "fourOfAKind" $
    it "tells if a hand is four of a kind" $ do
      fourOfAKind [(3, 0), (3, 1), (3, 2), (3, 3), (5, 3)] `shouldBe` Just (3, 3)
      fourOfAKind [(3, 0), (3, 1), (3, 2), (4, 3), (5, 3)] `shouldBe` Nothing

  describe "threeOfAKind" $
    it "tells if a hand is four of a kind" $ do
      threeOfAKind [(3, 0), (3, 1), (3, 2), (3, 3), (5, 3)] `shouldBe` Nothing
      threeOfAKind [(3, 0), (3, 1), (3, 2), (4, 3), (5, 3)] `shouldBe` Just (3, 2)

  describe "twoPairs" $
    it "tells if a hand is two pairs" $ do
      twoPairs [(3, 0), (3, 1), (4, 2), (4, 3), (5, 3)] `shouldBe` Just [(4, 3), (3, 1)]
      twoPairs [(3, 0), (2, 1), (4, 2), (4, 3), (5, 3)] `shouldBe` Nothing

  describe "onePair" $
    it "tells if a hand is one pair" $ do
      onePair [(3, 0), (3, 1), (4, 2), (4, 3), (5, 3)] `shouldBe` Nothing
      onePair [(3, 0), (2, 1), (4, 2), (4, 3), (5, 3)] `shouldBe` Just (4, 3)

  describe "fullHouse" $
    it "tells if a hand is full house" $ do
      fullHouse [(3, 0), (3, 1), (4, 1), (4, 2), (4, 3)] `shouldBe` Just [(4, 3), (3, 1)]
      fullHouse [(2, 0), (3, 1), (4, 1), (4, 2), (4, 3)] `shouldBe` Nothing
      fullHouse [(3, 0), (3, 1), (4, 1), (4, 2), (10, 3)] `shouldBe` Nothing

  describe "evaluateHand" $
    it "evaluates a hand" $ do
      evaluateHand [(5,0),(5,2),(6,3),(7,3),(13,1)] < evaluateHand [(2,0),(3,3),(8,1),(8,3),(10,1)]
        `shouldBe` True
      evaluateHand [(4,1),(6,3),(9,2),(12,0),(12,2)] > evaluateHand [(3,1),(6,1),(7,2),(12,1),(12,3)]
        `shouldBe` True
