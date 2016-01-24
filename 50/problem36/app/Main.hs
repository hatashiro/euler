module Main where

import Euler

main :: IO ()
main = print $ sum $ filter isDoubleBasePalindrome [1..1000000]
