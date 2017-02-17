module Problem30 where

import Data.Char

digits :: Int -> [Int]
digits n = map digitToInt $ show n

isDigitNthPower :: Int -> Int -> Bool
isDigitNthPower _ 1 = False
isDigitNthPower n x = x == sum (map (^n) $ digits x)

main :: IO ()
main = print . sum $ filter (isDigitNthPower 5) [1..1000000]
