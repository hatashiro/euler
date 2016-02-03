module Euler where

import Data.Ratio

cornerNumbers :: Int -> [Int]
cornerNumbers 1 = [1]
cornerNumbers n = [(first+diff), (first+2*diff) .. (first+4*diff)]
  where first = (2*n-3)^2
        diff = 2*n-2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not (any divisible [2..r])
  where r = floor (sqrt $ fromIntegral n)
        divisible r = n `mod` r == 0

numCornerPrimes :: Int -> Int
numCornerPrimes n = length $ filter isPrime (cornerNumbers n)

spiralPrimeRatio :: Int -> Rational
spiralPrimeRatio n = fromIntegral (numSpiralPrimes n) % fromIntegral (4*n - 3)
  where
    numSpiralPrimes = (map numSpiralPrimes' [0..] !!)
      where
        numSpiralPrimes' 1 = 0
        numSpiralPrimes' n = numSpiralPrimes (n-1) + numCornerPrimes n
