module Main where

import Euler

main :: IO ()
main = print $
  product $ map (irrationalSequence !!) [0, 9, 99, 999, 9999, 99999, 999999]
