module Euler where

import Data.List
import Data.Maybe

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not $ any divisible [2..root]
  where root = floor $ sqrt (fromIntegral n)
        divisible x = n `mod` x == 0

leftTruncatablePrimesWithDigit :: Int -> [Int]
leftTruncatablePrimesWithDigit = (map leftTruncatablePrimesWithDigit' [0..] !!)
  where
    leftTruncatablePrimesWithDigit' 0 = [0]
    leftTruncatablePrimesWithDigit' digit
      | null primes = []
      | otherwise   = filter isPrime leftConcats
        where primes = leftTruncatablePrimesWithDigit (digit - 1)
              leftConcats = [x * 10^(digit-1) + y | x <- [1..9], y <- primes]

rightTruncatablePrimesWithDigit :: Int -> [Int]
rightTruncatablePrimesWithDigit = (map rightTruncatablePrimesWithDigit [0..] !!)
  where
    rightTruncatablePrimesWithDigit 0 = [0]
    rightTruncatablePrimesWithDigit digit
      | null primes = []
      | otherwise   = filter isPrime rightConcats
        where primes = rightTruncatablePrimesWithDigit (digit - 1)
              rightConcats = [x + y * 10 | x <- [1..9], y <- primes]

truncatablePrimesWithDigit :: Int -> Maybe [Int]
truncatablePrimesWithDigit n
  | null right = Nothing
  | otherwise  = Just (left `intersect` right)
  where left = leftTruncatablePrimesWithDigit n
        right = rightTruncatablePrimesWithDigit n

truncatablePrimes :: [Int]
truncatablePrimes = foldl1 (++) primeList
  where maybePrimeList = takeWhile isJust (map truncatablePrimesWithDigit [2..])
        primeList = map fromJust maybePrimeList
