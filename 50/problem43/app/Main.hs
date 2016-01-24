module Main where

import Data.Digits
import Euler

main :: IO ()
main = print.sum $ map (unDigits 10) (filter isSubstringDivisible pandigitals)
