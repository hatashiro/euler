module Problem35 where

import Data.Char

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not (any divisible [2..root])
  where
    divisible x = n `mod` x == 0
    root = floor (sqrt $ fromIntegral n)

digits :: Int -> [Int]
digits n = map digitToInt (show n)

unDigits :: [Int] -> Int
unDigits [] = 0
unDigits xs = read $ map intToDigit xs

getCircularNumbers :: Int -> [Int]
getCircularNumbers x = map unDigits $ take (length ds) (iterate next ds)
  where ds = digits x
        next (x:xs) = xs ++ [x]

main :: IO ()
main = print $ length $ filter isCircularPrime [2..1000000]
  where isCircularPrime n = all isPrime (getCircularNumbers n)
