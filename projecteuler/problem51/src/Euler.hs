module Euler where

isPrime :: Integer -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor (sqrt (fromIntegral n))
        divisible m = n `mod` m == 0
