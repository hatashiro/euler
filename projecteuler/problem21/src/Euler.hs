module Euler
( d
, isAmicable
) where

divisorSumWithDivisor :: Int -> Int -> Int
divisorSumWithDivisor 0 _ = 0
divisorSumWithDivisor 1 _ = 0
divisorSumWithDivisor n divisor
  | divisor == 1 = 1 + (divisorSumWithDivisor n 2)
  | divisor < root = if n `mod` divisor == 0
                        then divisor + (n `div` divisor) + (divisorSumWithDivisor n (divisor+1))
                        else divisorSumWithDivisor n (divisor+1)
  | divisor == root = if n `mod` divisor == 0
                         then divisor
                         else 0
  | otherwise = 0
  where root = floor $ sqrt (fromIntegral n)

divisorSum :: Int -> Int
divisorSum n = divisorSumWithDivisor n 1

d :: Int -> Int
d = (map divisorSum [0..] !!)

isAmicable :: Int -> Bool
isAmicable n = divisorSum (divisorSum n) == n
