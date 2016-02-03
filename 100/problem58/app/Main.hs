module Main where

import Data.List
import Data.Ratio
import Euler

main :: IO ()
main = print $
  case find (< 0.1) [spiralPrimeRatio n | n <- [2..]] of
    Just n  -> 2 * ((denominator n + 3) `div` 4) - 1
    Nothing -> -1
