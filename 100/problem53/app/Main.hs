module Main where

import Euler

main :: IO ()
main = print $
  length $ filter (>1000000) [combination n r | n <- [1..100], r <- [1..n]]
