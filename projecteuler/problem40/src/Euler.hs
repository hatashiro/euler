module Euler where

import Data.Digits

irrationalSequence :: [Int]
irrationalSequence = concatMap (digits 10) [1..]
