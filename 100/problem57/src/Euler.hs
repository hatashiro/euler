module Euler where

import Data.Digits
import Data.Ratio

data Fraction = Fraction Integer Integer deriving (Eq, Show)

instance Num Fraction where
  Fraction a b + Fraction c d = normalize (Fraction (a*d + b*c) (b*d))
  Fraction a b * Fraction c d = normalize (Fraction (a*c) (b*d))
  abs (Fraction a b) = Fraction (abs a) b
  signum (Fraction a b) = Fraction (signum a) 1
  fromInteger i = Fraction (fromInteger i) 1
  negate (Fraction a b) = Fraction (-a) b

instance Fractional Fraction where
  recip (Fraction a b) = normalize (Fraction b a)
  fromRational r = normalize (Fraction (fromIntegral $ numerator r) (fromIntegral $ denominator r))

toFloat :: Fraction -> Float
toFloat (Fraction a b) = fromIntegral a / fromIntegral b

normalize :: Fraction -> Fraction
normalize (Fraction a b) = Fraction (sig*numerator) denominator
  where
    sig = signum a * signum b
    absA = abs a
    absB = abs b
    numerator   = absA `div` gcd absA absB
    denominator = absB `div` gcd absA absB

sqrtIteration :: Int -> Fraction
sqrtIteration = (map sqrtIteration' [0..] !!)
  where
    sqrtIteration' 1 = Fraction 3 2
    sqrtIteration' n = 1 + 1 / (1 + sqrtIteration (n-1))

hasLongerNumerator :: Fraction -> Bool
hasLongerNumerator (Fraction a b) = length (digits 10 a) > length (digits 10 b)
