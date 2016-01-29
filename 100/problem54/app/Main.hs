module Main where

import Data.List.Split
import Euler

main :: IO ()
main = do
  content <- readFile "p054_poker.txt"
  let lines = filter (/="") (splitOn "\n" content)
  print.length $
    filter (\(x, y) -> evaluateHand x > evaluateHand y)
           (map parseHands lines)
