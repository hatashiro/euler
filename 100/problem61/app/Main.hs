module Main where

import Euler

main :: IO ()
main = print $
  case head (cyclicalFigurate6 [isTriangle,
                                isSquare,
                                isPentagonal,
                                isHexagonal,
                                isHeptagonal,
                                isOctagonal]) of
  (a, b, c, d, e, f) -> sum [a, b, c, d, e, f]
