module Euler where

import Data.Digits
import Data.List

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor (sqrt (fromIntegral n))
        divisible m = n `mod` m == 0

primes :: [Int]
primes = filter isPrime [2..]

findDigitPositions :: Int -> Int -> [Int]
findDigitPositions n d = elemIndices d (digits 10 n)

replaceDigitPositions :: Int -> [Int] -> Int -> Int
replaceDigitPositions n indices d = unDigits 10 $
  helper (digits 10 n) indices d
    where
      helper ds []     _ = ds
      helper ds (i:is) d =
        take i ds ++ d:helper (drop (i+1) ds) (map (subtract (i+1)) is) d

nPrimeFamily :: Int -> Maybe Int
nPrimeFamily n = find (helper n) primes
  where
    helper n prime = any (havingNPrimeFamily n prime) [0..(10-n)]
    havingNPrimeFamily n prime digit =
      case findDigitPositions prime digit of
        [] -> False
        indices ->
          length (filter isPrime familyMembers) == n
            where
              familyMembers = map (replaceDigitPositions prime indices) [digit..9]
