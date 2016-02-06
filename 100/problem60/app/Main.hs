module Main where

import Data.Set (elems)
import Euler

main :: IO ()
main = print $ sum $ elems (primePairSet 5)
