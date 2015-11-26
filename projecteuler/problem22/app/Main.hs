module Main where

import Euler
import System.IO

main :: IO ()
main = do
  names >>= putStrLn . show . namesScore
