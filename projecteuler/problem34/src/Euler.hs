module Euler where

import Data.Char

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

digits :: Int -> [Int]
digits n = map digitToInt $ show n
