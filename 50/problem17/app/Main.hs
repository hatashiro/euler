module Main where

import Euler

main :: IO ()
main =
  putStrLn $ show $ foldl1 (+) $ map numberLetterCount [1..1000]
