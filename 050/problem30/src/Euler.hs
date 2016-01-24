module Euler where

import Data.Char

digits :: Int -> [Int]
digits n = map digitToInt $ show n

isDigitNthPower :: Int -> Int -> Bool
isDigitNthPower _ 1 = False
isDigitNthPower n x = x == sum (map (^n) $ digits x)
