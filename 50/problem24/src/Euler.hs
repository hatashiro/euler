module Euler
( fac
, deleteAt
, nthPermutation
) where

import Data.List

fac :: Int -> Int
fac n
  | n == 1 = 1
  | otherwise = n * fac (n-1)

deleteAt :: Int -> [a] -> [a]
deleteAt n xs =
  let (ys,zs) = splitAt n xs
    in ys ++ (tail zs)

nthPermutation :: [Int] -> Int -> [Int]
nthPermutation (a:[]) _ = [a]
nthPermutation arr nth =
  (sArr !! headIdx):(nthPermutation (deleteAt headIdx sArr) tailNth)
  where
    sArr = sort arr
    tailSizeFac = fac ((length arr) - 1)
    headIdx = nth `div` tailSizeFac
    tailNth = nth `mod` tailSizeFac
