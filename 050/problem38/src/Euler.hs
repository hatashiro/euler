module Euler where

import Data.Digits
import Data.List

pandigitalNumbers :: [Int] -> [Int]
pandigitalNumbers [a] = [a]
pandigitalNumbers xs  = [x*10^(len-1) + y | let len = length xs,
                                            x <- reverse xs,
                                            y <- pandigitalNumbers (delete x xs)]

isConcatenatedMultiple :: Int -> Bool
isConcatenatedMultiple n = any (`canFormConcatenatedMultiple` ds) multipliers
  where ds = digits 10 n
        multipliers = map (\x -> unDigits 10 (take x ds)) [1..(length ds`div`2)]

canFormConcatenatedMultiple :: Int -> [Int] -> Bool
canFormConcatenatedMultiple m ds = helper m ds 1
  where helper _ [] _ = True
        helper m ds n
          | mdsLen > length ds = False
          | otherwise          = take mdsLen ds == mds &&
                                   helper m (drop mdsLen ds) (n+1)
          where mds = digits 10 (m*n)
                mdsLen = length mds
