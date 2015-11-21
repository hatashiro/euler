module Main where

import Euler

main :: IO ()
main =
  putStrLn.show $ foldl1 (+) $ map (\x -> if isAmicable x then x else 0) [1..10000]
