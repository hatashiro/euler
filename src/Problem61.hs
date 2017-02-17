module Problem61 where

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

cyclicalFigurate6 :: [Int -> Bool] -> [(Int, Int, Int, Int, Int, Int)]
cyclicalFigurate6 fs = concatMap cyclicalFigurate6' (permutations fs)
  where
    cyclicalFigurate6' [f1, f2, f3, f4, f5, f6] =
      [(a, b, c, d, e, f) | p1 <- [11..99],
                            p2 <- [11..99],
                            let a = p1 * 100 + p2,
                            f1 a,
                            p3 <- [11..99],
                            let b = p2 * 100 + p3,
                            f2 b,
                            p4 <- [11..99],
                            let c = p3 * 100 + p4,
                            f3 c,
                            p5 <- [11..99],
                            let d = p4 * 100 + p5,
                            f4 d,
                            p6 <- [11..99],
                            let e = p5 * 100 + p6,
                            f5 e,
                            let f = p6 * 100 + p1,
                            f6 f]

main :: IO ()
main = print $
  case head (cyclicalFigurate6 [isTriangle,
                                isSquare,
                                isPentagonal,
                                isHexagonal,
                                isHeptagonal,
                                isOctagonal]) of
  (a, b, c, d, e, f) -> sum [a, b, c, d, e, f]
