module Main where

import Euler
import System.IO

main :: IO ()
main = interact (show . namesScore . parseNames)
