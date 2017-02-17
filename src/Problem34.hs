module Problem34 where

import Data.Char

factorial :: Int -> Int
factorial = (map fac [0..] !!)
  where
    fac 0 = 1
    fac n = n * factorial (n-1)

digits :: Int -> [Int]
digits n = map digitToInt $ show n

main :: IO ()
main = print.sum $ filter (\x -> x == sum (map factorial (digits x))) [10..1000000]
