module Problem44 where

import Data.List

pentagonNumber :: Int -> Int
pentagonNumber n = n * (3*n - 1) `div` 2

isqrt :: Int -> Maybe Int
isqrt n = binarySearch 1 n
  where
    binarySearch low high =
      let mid = (high + low) `div` 2
          midSquare = mid * mid in
      if low > high then Nothing
      else if n == midSquare then Just mid
      else if n < midSquare then binarySearch low (mid-1)
      else binarySearch (mid+1) high

isPentagonNumber :: Int -> Bool
isPentagonNumber p =
  case isqrt (1 + 24*p) of
    Just n  -> n `mod` 6 == 5
    Nothing -> False

isSumPentagon :: (Int, Int) -> Bool
isSumPentagon (a, b) = isPentagonNumber (aPen + bPen)
  where aPen = pentagonNumber a
        bPen = pentagonNumber b

isDiffPentagon :: (Int, Int) -> Bool
isDiffPentagon (a, b) = isPentagonNumber (aPen - bPen)
  where aPen = pentagonNumber a
        bPen = pentagonNumber b

main :: IO ()
main = print $
  case find (\p -> isSumPentagon p && isDiffPentagon p)
       [(x, y) | x <- [2..], y <- [1..(x-1)]] of
       Just (a, b) -> pentagonNumber a - pentagonNumber b
       Nothing     -> -1
