module Main where

import Euler

main :: IO ()
main = print $ length $ filter isCircularPrime [2..1000000]
  where isCircularPrime n = all isPrime (getCircularNumbers n)
