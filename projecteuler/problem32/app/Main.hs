module Main where

import Data.List
import Euler

getPandigitalProduct :: (Int, Int) -> [Int]
getPandigitalProduct (xDigit, yDigit) =
  [z | x <- integersFromListWithDigit xDigit [1..9],
       let yDigits = diff [1..9] $ digits x,
       y <- integersFromListWithDigit yDigit yDigits,
       let zDigits = diff yDigits $ digits y,
       let z = x * y,
       consistOf z zDigits]

removeDuplication :: (Ord a) => [a] -> [a]
removeDuplication = map head . group . sort

main :: IO ()
main = print $
  sum $ removeDuplication $ foldl1 (++) $ map getPandigitalProduct digitPairs
  where digitPairs = multipliableDigitsInSum 10
