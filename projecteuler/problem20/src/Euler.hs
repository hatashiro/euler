module Euler
( factorial
, digitSum
) where

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * (factorial $ n-1)

digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n = (n `mod` 10) + (digitSum $ n `div` 10)
