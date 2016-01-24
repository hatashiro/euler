module Euler where

import Data.Maybe

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor $ sqrt (fromIntegral n)
        divisible m = n `mod` m == 0

primes :: [Int]
primes = filter isPrime [2..]

primesLte :: Int -> [Int]
primesLte n = takeWhile (<= n) primes

oddComps :: [Int]
oddComps = filter (\x -> odd x && (not.isPrime) x) [3..]

isqrt :: Int -> Maybe Int
isqrt n = binarySearch 1 n
  where
    binarySearch low high =
      let mid = (low + high) `div` 2
          midSqr = mid * mid in
      if low > high then Nothing
      else if midSqr == n then Just mid
      else if midSqr > n then binarySearch low (mid-1)
      else binarySearch (mid+1) high

goldbachConjecture :: Int -> Bool
goldbachConjecture n = any (isJust.isqrt.(`div`2).(n-)) (primesLte n)
