module Euler
( divSum
, isAbundant
, isAbundantSum
) where

import Data.List
import Data.Maybe

divSum :: Int -> Int
divSum n = foldl
             (\res a ->
               if n `mod` a == 0
                 then
                   let a' = n `div` a
                   in
                     if a == a' || a' == n
                       then a + res
                       else a + a' + res
                 else res)
             0 [1..(floor $ sqrt $ fromIntegral n)]

isAbundant :: Int -> Bool
isAbundant n = divSum n > n

isAbundantMemo :: Int -> Bool
isAbundantMemo = (map isAbundant [0..] !!)

abundances :: [Int]
abundances = [x | x <- [1..], isAbundantMemo x]

isAbundantSum :: Int -> Bool
isAbundantSum n = isJust $ find (isAbundant.(n-)) $ takeWhile (<n) abundances
