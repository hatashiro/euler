module Euler where

pentagonNumber :: Int -> Int
pentagonNumber n = n * (3*n - 1) `div` 2

isPentagonNumber :: Int -> Bool
isPentagonNumber n = isPentagonNumber' n 1
  where
    isPentagonNumber' n i = n == ith || (ith < n && isPentagonNumber' n (i+1))
      where ith = pentagonNumber i

isSumPentagon :: (Int, Int) -> Bool
isSumPentagon (a, b) = isPentagonNumber (aPen + bPen)
  where aPen = pentagonNumber a
        bPen = pentagonNumber b

isDiffPentagon :: (Int, Int) -> Bool
isDiffPentagon (a, b) = isPentagonNumber (aPen - bPen)
  where aPen = pentagonNumber a
        bPen = pentagonNumber b
