module Main where

import Euler

main :: IO ()
main = do
  putStrLn.show $ sum [n | n <- [1..28123], (not.isAbundantSum) n]
