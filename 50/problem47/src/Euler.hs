module Euler where

import Data.List

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor $ sqrt (fromIntegral n)
        divisible m = n `mod` m == 0

primes :: [Int]
primes = filter isPrime [2..]

factorize :: Int -> [Int]
factorize n = factorize' n []
  where
    divisible n m = n `mod` m == 0
    factorize' 1 fs = fs
    factorize' n fs =
      let ps = takeWhile (<=n) primes in
      case find (divisible n) primes of
        Just m  -> factorize' (n`div`m) (if (not.null) fs && m == head fs
                                         then fs
                                         else m:fs)
        Nothing -> error "there should be at least one divisible integer"

nDistinctPrimeFactors :: Int -> Int -> Bool
nDistinctPrimeFactors n i =
  all (\fs -> length fs == n) factorizations &&
    null (foldl1 intersect factorizations)
  where factorizations = map factorize [i..(i+n-1)]
