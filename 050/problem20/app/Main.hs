module Main where

import Euler

main :: IO ()
main =
  putStrLn . show $ digitSum $ factorial 100
