module Euler where

import Data.Digits

lastDigits :: Int -> Integer -> [Integer]
lastDigits n x = lastN n (digits 10 x)
  where lastN n xs = drop (length xs - n) xs
