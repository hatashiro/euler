module Euler where

import Data.List

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

truncatablePrimes' n =
        let leftTruncatablePrimes = leftTruncatablePrimesWithDigit n
            rightTruncatablePrimes = rightTruncatablePrimesWithDigit n in
        leftTruncatablePrimes `intersect` rightTruncatablePrimes

truncatablePrimes :: [Int]
truncatablePrimes = foldl1 (++) (map truncatablePrimes' [2..6])
