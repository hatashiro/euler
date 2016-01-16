import Euler
import Test.Hspec

import Data.Char

main :: IO ()
main = hspec $
  describe "irrationalSequence" $
    it "returns an irrational sequence" $ do
      irrationalSequence !! 11 `shouldBe` 1
      take 33 irrationalSequence `shouldBe`
        map digitToInt "123456789101112131415161718192021"
