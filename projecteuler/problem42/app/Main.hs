module Main where

import Euler

main :: IO ()
main = do
  str <- readFile "words.txt"
  print $ length $ filter (isTriangleNumber.getWordValue) (parseWords str)
