module Main where

import Euler

main :: IO ()
main = print $
  maximum [digitSum (a^b) | a <- [1..100], b <- [1..100]]
