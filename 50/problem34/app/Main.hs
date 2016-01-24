module Main where

import Euler

main :: IO ()
main = print.sum $ filter (\x -> x == sum (map factorial (digits x))) [10..1000000]
