import Data.Bits
import Data.Char
import Euler
import Test.Hspec

toCodes :: String -> [Int]
toCodes = map ord

main :: IO ()
main = hspec $ do
  describe "possiblePasswords" $
    it "returns all possible passwords" $ do
      possiblePasswords `shouldContain` ["abc"]
      possiblePasswords `shouldContain` ["skx"]
      possiblePasswords `shouldContain` ["zpd"]
      possiblePasswords `shouldContain` ["zyx"]

  describe "parseIntList" $
    it "parses an integer list from a string" $
      parseIntList "13,125,15" `shouldBe` [13, 125, 15]

  describe "encrypt" $
    it "encrypts a string with a key" $
      encrypt "hello" "abc" `shouldBe`
        [cxor 'h' 'a', cxor 'e' 'b', cxor 'l' 'c', cxor 'l' 'a', cxor 'o' 'b']

  describe "decrypt" $
    it "decrypts an encrypted string" $ do
      decrypt (encrypt "hello" "abc") "abc" `shouldBe` Just "hello"
      decrypt (encrypt "hello" "lql") "abc" `shouldBe` Nothing
