module Euler where

import Data.Digits
import Data.List

ndigitPandigitals :: Int -> [Int]
ndigitPandigitals n = map (unDigits 10) (permutations [1..n])

pandigitals :: [Int]
pandigitals = concatMap ndigitPandigitals [1..9]

isPrime :: Int -> Bool
isPrime n = all undivisable [2..root]
  where root = floor $ sqrt (fromIntegral n)
        undivisable x = n `mod` x /= 0
