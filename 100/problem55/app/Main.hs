module Main where

import Data.Maybe
import Euler

main :: IO ()
main = print $
  length $ filter isNothing (map itersForPalindrome [1..10000])
