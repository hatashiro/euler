module Main where

import Euler
import Data.List

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main :: IO ()
main = putStrLn.show $ length $ rmdups [a ^ b | a <- [2..100], b <- [2..100]]
