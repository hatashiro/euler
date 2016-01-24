module Main where

import Euler

main :: IO ()
main = print $ maximum $ filter isPrime pandigitals
