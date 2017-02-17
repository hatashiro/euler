module Problem36 where

import Data.Digits

isPalindrome :: [Int] -> Bool
isPalindrome [_] = True
isPalindrome xs = isPalindrome' xs []
  where
    isPalindrome' xs@(h:t) rxs
      | length xs > length rxs = isPalindrome' t (h:rxs)
      | length xs < length rxs = isPalindrome' xs (tail rxs)
      | otherwise              = xs == rxs

isDoubleBasePalindrome :: Int -> Bool
isDoubleBasePalindrome n = isPalindrome (digits 10 n) && isPalindrome (digits 2 n)

main :: IO ()
main = print $ sum $ filter isDoubleBasePalindrome [1..1000000]
