module Problem56 where

import Data.Digits

digitSum :: Integer -> Integer
digitSum n = sum (digits 10 n)

main :: IO ()
main = print $
  maximum [digitSum (a^b) | a <- [1..100], b <- [1..100]]
