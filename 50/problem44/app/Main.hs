module Main where

import Data.List
import Euler

main :: IO ()
main = print $
  case find (\p -> isSumPentagon p && isDiffPentagon p)
       [(x, y) | x <- [2..], y <- [1..(x-1)]] of
       Just (a, b) -> pentagonNumber a - pentagonNumber b
       Nothing     -> -1
