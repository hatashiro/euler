module Main where

import Euler

maxN :: (Ord a, Ord b) => (a -> b) -> [a] -> a
maxN f xs = snd . maximum $ zip (map f xs) xs

main :: IO ()
main = print $ maxN (length.pythagorianTriples) [3..1000]
