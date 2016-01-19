module Euler where

isqrt :: Integer -> Maybe Integer
isqrt n = binarySearch 1 n
  where
    binarySearch low high =
      let mid = (low + high) `div` 2
          midSq = mid * mid in
      if high < low then Nothing
      else if midSq == n then Just mid
      else if midSq > n then binarySearch low (mid-1)
      else binarySearch (mid+1) high

isTriangle :: Integer -> Bool
isTriangle t =
  case isqrt (1 + 8 * t) of
    Just n  -> (n - 1) `mod` 2 == 0
    Nothing -> False

isPentagonal :: Integer -> Bool
isPentagonal p =
  case isqrt (1 + 24 * p) of
    Just n  -> (n + 1) `mod` 6 == 0
    Nothing -> False

isHexagonal :: Integer -> Bool
isHexagonal h =
  case isqrt (1 + 8 * h) of
    Just n  -> (n + 1) `mod` 4 == 0
    Nothing -> False

hexagonals :: [Integer]
hexagonals = map h [1..]
  where h n = n * (2*n - 1)
