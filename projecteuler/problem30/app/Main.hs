module Main where

import Euler

main :: IO ()
main = print . sum $ filter (isDigitNthPower 5) [1..1000000]
