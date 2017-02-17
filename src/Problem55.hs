module Problem55 where

import Data.Digits
import Data.Maybe

isPalindrome :: Integer -> Bool
isPalindrome n = isPalindrome' (digits 10 n) []
  where
    isPalindrome' [_]         [] = True
    isPalindrome' xs@(x':xs') ys
      | length xs == length ys = xs == ys
      | length xs <  length ys = xs == tail ys
      | otherwise              = isPalindrome' xs' (x':ys)

reverseAdd :: Integer -> Integer
reverseAdd n = n + (unDigits 10.reverse.digits 10) n

itersForPalindrome :: Integer -> Maybe Int
itersForPalindrome n = itersForPalindrome' (reverseAdd n) 1
  where
    itersForPalindrome' result iters
      | iters > 50          = Nothing
      | isPalindrome result = Just iters
      | otherwise           = itersForPalindrome' (reverseAdd result) (iters+1)

main :: IO ()
main = print $
  length $ filter isNothing (map itersForPalindrome [1..10000])
