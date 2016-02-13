module Euler where

import Data.Function.Memoize

isqrt = floor . sqrt . fromIntegral
isSquare n = isqrt n ^ 2 == n

memoSqrtFracPair :: Int -> Int -> (Int, Int)
memoSqrtFracPair = memoize2 sqrtFracPair

sqrtFracPair :: Int -> Int -> (Int, Int)
sqrtFracPair n 0 = (isqrt n, 1)
sqrtFracPair n i =
  let (x, y) = memoSqrtFracPair n (i-1)
      denom  = (n - x^2) `div` y
      digit  = (isqrt n + x) `div` denom
      numer  = -(x - digit * denom) in
  (numer, denom)

cyclingLength :: (Eq a) => [a] -> Int
cyclingLength []     = 0
cyclingLength (x:xs) = helper xs x
  where
    helper (x:xs) first
      | x == first = 1
      | otherwise  = 1 + helper xs first

sqrtFracPeriod :: Int -> Int
sqrtFracPeriod n
  | isSquare n = 0
  | otherwise  = cyclingLength $ map (memoSqrtFracPair n) [0..]
