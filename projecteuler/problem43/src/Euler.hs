module Euler where

import Data.Digits
import Data.List

pandigitals :: [[Int]]
pandigitals = filter ((/= 0).head) (permutations [0..9])

sublist :: Int -> Int -> [a] -> [a]
sublist idx len = take len . drop idx

isSubstringDivisible :: [Int] -> Bool
isSubstringDivisible xs =
  unDigits 10 (sublist 1 3 xs) `mod` 2 == 0 &&
  unDigits 10 (sublist 2 3 xs) `mod` 3 == 0 &&
  unDigits 10 (sublist 3 3 xs) `mod` 5 == 0 &&
  unDigits 10 (sublist 4 3 xs) `mod` 7 == 0 &&
  unDigits 10 (sublist 5 3 xs) `mod` 11 == 0 &&
  unDigits 10 (sublist 6 3 xs) `mod` 13 == 0 &&
  unDigits 10 (sublist 7 3 xs) `mod` 17 == 0
