module Euler where

import Prime
import Data.Function.Memoize
import Data.Ratio

cornerNumbers :: Integer -> [Integer]
cornerNumbers 1 = [1]
cornerNumbers n = [(first+diff), (first+2*diff) .. (first+4*diff)]
  where first = (2*n-3)^2
        diff = 2*n-2

numCornerPrimes :: Integer -> Integer
numCornerPrimes n = fromIntegral.length $ filter isPrime (cornerNumbers n)

numSpiralPrimes = memoize numSpiralPrimes'

numSpiralPrimes' 1 = 0
numSpiralPrimes' n = numSpiralPrimes (n-1) + numCornerPrimes n

spiralPrimeRatio :: Integer -> Rational
spiralPrimeRatio n = numSpiralPrimes n % (4*n - 3)
