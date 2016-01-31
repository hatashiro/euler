module Euler where

import Data.Digits

digitSum :: Integer -> Integer
digitSum n = sum (digits 10 n)
