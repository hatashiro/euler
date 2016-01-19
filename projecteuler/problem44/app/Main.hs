module Main where

import Data.List
import Euler

main :: IO ()
main = print $
  find (\p -> isSumPentagon p && isDiffPentagon p)
       [(x, y) | x <- [2..10000], y <- [1..(x-1)]]
