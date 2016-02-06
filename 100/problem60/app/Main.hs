module Main where

import Data.Set (elems)
import Euler

main :: IO ()
main = print $ elems (primePairSet 5)
