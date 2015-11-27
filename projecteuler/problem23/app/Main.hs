module Main where

import Euler

main :: IO ()
main = do
  putStrLn.show $ sum $ map (\x -> if isAbundantSum x then 0 else x) [0..28123]
