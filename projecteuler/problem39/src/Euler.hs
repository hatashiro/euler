module Euler where

isPythagoreanTriples :: Int -> Int -> Int -> Bool
isPythagoreanTriples a b c = a^2 + b^2 == c^2

pythagorianTriples :: Int -> [(Int, Int, Int)]
pythagorianTriples n = [(a, b, c) | a <- [1..(n`div`3)],
                                    let rest = n - a,
                                    b <- [a..(rest`div`2)],
                                    let c = rest - b,
                                    isPythagoreanTriples a b c]
