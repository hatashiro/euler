module Main where

import Euler

main :: IO ()
main = print $
  length $ filter (odd.sqrtFracPeriod) [2..10000]
