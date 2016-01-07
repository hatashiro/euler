module Main where

import Euler

main :: IO ()
main = print $
  let fractions = [Fraction a b | b <- [11..99], a <- [10..(b-1)]]
  in
    minimise $ foldl1 fracMul $ filter isNontriviallyCurious fractions
