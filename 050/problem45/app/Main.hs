module Main where

import Data.List
import Euler

cond n = isTriangle n && isPentagonal n && n > 40755

main :: IO ()
main = print $ find cond hexagonals
