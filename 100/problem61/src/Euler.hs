module Euler where

import Data.List

isqrt :: Int -> Maybe Int
isqrt n
  | r^2 == n  = Just r
  | otherwise = Nothing
    where r = floor $ sqrt (fromIntegral n)

isPolygonal :: Int -> Int -> Int -> Int -> Bool
isPolygonal a b c x =
  case isqrt (4*a*c*x + b^2) of
    Just r  -> (r - b) `mod` (2*a) == 0
    Nothing -> False

isTriangle   = isPolygonal 1 1 2
isSquare     = isPolygonal 1 0 1
isPentagonal = isPolygonal 3 (-1) 2
isHexagonal  = isPolygonal 2 (-1) 1
isHeptagonal = isPolygonal 5 (-3) 2
isOctagonal  = isPolygonal 3 (-2) 1

cyclicalFigurate3 :: [Int -> Bool] -> [(Int, Int, Int)]
cyclicalFigurate3 fs = concatMap cyclicalFigurate3' (permutations fs)
  where
    cyclicalFigurate3' [f1, f2, f3] = [(x, y, z) | p1 <- [11..99],
                                                   p2 <- [11..99],
                                                   let x = p1 * 100 + p2,
                                                   f1 x,
                                                   p3 <- [11..99],
                                                   let y = p2 * 100 + p3,
                                                   f2 y,
                                                   let z = p3 * 100 + p1,
                                                   f3 z]
