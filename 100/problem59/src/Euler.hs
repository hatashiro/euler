module Euler where

import Data.Bits (xor)
import Data.Char (chr, isAlphaNum, ord)
import Data.List (cycle)
import Data.List.Utils (split)

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
