module Main where

import Data.List
import Euler

main :: IO ()
main = print $
  find (nDistinctPrimeFactors 4) [644..]
