module Main where

import Euler

main :: IO ()
main =
  putStrLn.show.sum $ map (\x -> if isAmicable x then x else 0) [1..9999]
