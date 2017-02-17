module Problem49 where

import Data.Digits hiding (digits)
import Data.List

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where divisible m = n `mod` m == 0
        r = floor (sqrt $ fromIntegral n)

areInTheSameInterval :: Int -> Int -> Int -> Bool
areInTheSameInterval a b c = (a + c) == 2 * b

findPrimePermutation :: [Int] -> [(Int, Int, Int)]
findPrimePermutation ds = [(a, b, c) | a <- xs, isPrime a,
                                       b <- xs, b > a, isPrime b,
                                       c <- xs, c > b, isPrime c,
                                       areInTheSameInterval a b c]
  where xs = map (unDigits 10) (permutations ds)

digits :: [[Int]]
digits = [[a, b, c, d] | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9]]

main :: IO ()
main = print $
  concatMap findPrimePermutation digits
