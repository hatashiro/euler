module Euler where

import Data.Char
import Data.List

data Fraction = Fraction Int Int deriving (Show)

instance Eq Fraction where
  (Fraction a b) == (Fraction c d) = a * d == b * c

digits :: Int -> [Int]
digits n = map digitToInt (show n)

fromDigits :: [Int] -> Int
fromDigits xs = read $ map intToDigit xs

cancelDigit :: Int -> Int -> Int
cancelDigit n d = fromDigits $ delete d (digits n)

commonDigits :: Int -> Int -> [Int]
commonDigits a b = digits a `intersect` digits b

minimise :: Fraction -> Fraction
minimise (Fraction a b) = Fraction (a `div` g) (b `div` g)
  where g = gcd a b

isNontriviallyCurious :: Fraction -> Bool
isNontriviallyCurious f@(Fraction x y) = any
  (\d -> d /= 0 && f == Fraction (cancelDigit x d) (cancelDigit y d))
  (commonDigits x y)

fracMul :: Fraction -> Fraction -> Fraction
Fraction a b `fracMul` Fraction c d = Fraction (a*c) (b*d)
