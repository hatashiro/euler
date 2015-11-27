module Euler
( properDivisors
, isAbundant
, isAbundantSum
) where

import Data.List

properDivisors :: Int -> [Int]
properDivisors n = foldl
                     (\res a ->
                       if n `mod` a == 0
                         then
                           let a' = n `div` a
                           in
                             if a == a' || a' == n
                               then [a] ++ res
                               else [a, a'] ++ res
                         else res)
                     [] [1..(floor $ sqrt $ fromIntegral n)]

isAbundant :: Int -> Bool
isAbundant n = (sum $ properDivisors n) > n

isAbundantMemo :: Int -> Bool
isAbundantMemo = (map isAbundant [0..] !!)

isAbundantSum :: Int -> Bool
isAbundantSum n = any (\x -> (isAbundantMemo x && isAbundantMemo (n-x)))
                      [1..n`div`2]
