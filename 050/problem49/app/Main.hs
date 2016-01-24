module Main where

import Euler

main :: IO ()
main = print $
  concatMap findPrimePermutation digits
