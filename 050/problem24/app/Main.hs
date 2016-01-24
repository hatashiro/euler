module Main where

import Euler

main :: IO ()
main = putStrLn $ foldl1 (++) $ map show $ nthPermutation [0..9] (1000000 - 1)
