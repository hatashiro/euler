module Euler
( getFractionPart
, getRecurringCycle
) where

import Data.List

getFraction :: Int -> Int -> [Int]
getFraction 0 _ = []
getFraction num divi =
  (nextNum `div` divi) : (getFraction (nextNum `mod` divi) divi)
  where nextNum = 10 * num

getFractionPart :: Int -> [Int]
getFractionPart n = getFraction 1 n

getRecurringCycle :: Int -> [Int]
getRecurringCycle n = getRecurringCycleWithNum 1 n [] []

getRecurringCycleWithNum :: Int -> Int -> [Int] -> [Int] -> [Int]
getRecurringCycleWithNum 0 _ _ _ = []
getRecurringCycleWithNum num divi nums fr =
  case elemIndex num nums of
    Just idx -> drop idx fr
    Nothing  -> getRecurringCycleWithNum (nextNum `mod` divi)
                                         divi
                                         (nums ++ [num])
                                         (fr ++ [(nextNum `div` divi)])
    where nextNum = 10 * num
