module Euler where

import Data.Digits
import Data.Ratio
import GHC.Real

sqrtIteration :: Int -> Rational
sqrtIteration = (map sqrtIteration' [0..] !!)
  where
    sqrtIteration' 1 = 3 % 2
    sqrtIteration' n = 1 + 1 / (1 + sqrtIteration (n-1))

hasLongerNumerator :: Rational -> Bool
hasLongerNumerator (a :% b) = length (digits 10 a) > length (digits 10 b)
