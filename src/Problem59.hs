module Problem59 where

import Data.Bits (xor)
import Data.Char
import Data.Char (chr, isAlphaNum, ord)
import Data.List
import Data.List (cycle)
import Data.List.Utils (split)
import Data.Maybe

possiblePasswords :: [String]
possiblePasswords = [[x, y, z] | x <- ['a'..'z'], y <- ['a'..'z'], z <- ['a'..'z']]

isValidAscii :: Int -> Bool
isValidAscii 32 = True -- space
isValidAscii 44 = True -- ,
isValidAscii 46 = True -- .
isValidAscii 40 = True -- (
isValidAscii 41 = True -- )
isValidAscii 39 = True -- '
isValidAscii 59 = True -- ;
isValidAscii 33 = True -- !
isValidAscii n = isAlphaNum (chr n)

parseIntList :: String -> [Int]
parseIntList = map read . split ","

cxor :: Char -> Char -> Int
cxor a b = ord a `xor` ord b

encrypt :: String -> String -> [Int]
encrypt str key = zipWith cxor str (cycle key)

zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe _ []     _      = Just []
zipWithMaybe _ _      []     = Just []
zipWithMaybe f (a:as) (b:bs) =
  f a b >>= (\x -> zipWithMaybe f as bs >>= (\xs -> Just (x:xs)))

decrypt :: [Int] -> String -> Maybe String
decrypt codes key = zipWithMaybe decryptCode codes (cycle key)
  where
    decryptCode code char =
      let decrypted = code `xor` ord char in
      if isValidAscii decrypted
         then Just (chr decrypted)
         else Nothing

main :: IO ()
main = do
  raw <- readFile "p059_cipher.txt"
  let codes = parseIntList raw
  print $
    case find isJust $ map (decrypt codes) possiblePasswords of
      Just (Just str) -> sum (map ord str)
      Nothing  -> -1
