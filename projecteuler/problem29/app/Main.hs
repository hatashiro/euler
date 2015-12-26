module Main where

import Euler

main :: IO ()
main = putStrLn.show $
  length $ removeDuplication [a ^ b | a <- [2..100], b <- [2..100]]
