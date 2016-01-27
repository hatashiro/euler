module Main where

import Euler
import Data.List

main :: IO ()
main = print $
  find (permutedMultiples 6) [2..]
