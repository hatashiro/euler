module Main where

import Euler

main :: IO ()
main = print $
  length $ filter (hasLongerNumerator.sqrtIteration) [1..1000]
