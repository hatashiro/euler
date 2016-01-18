module Euler where

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [5, 10 ..],
                  let y = x `div` 5 * 4]

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

sumPentagonPairs :: [(Int, Int)]
sumPentagonPairs = sumPentagonPairs' 3
  where
    sumPentagonPairs' i =
      if pentagonNumber (i-1) + pentagonNumber (i-2) < pentagonNumber i
         then []
         else [(x, y) | x <- [2..(i-1)],
                        y <- [1..(x-1)],
                        isSumPentagon (x, y)] ++ sumPentagonPairs' (i+1)
