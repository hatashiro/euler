module Main where

import Euler
import Data.List

main :: IO ()
main = print $
  find isConcatenatedMultiple (pandigitalNumbers [1..9])
