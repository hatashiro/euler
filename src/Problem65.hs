module Problem65 where

import Data.Ratio

main :: IO ()
main = print $ digitSum (numerator $ e 100)

fk :: Int -> Integer
fk x = case x `mod` 3 of
         2 -> 2 * fromIntegral (x `div` 3 + 1)
         _ -> 1

fractions :: [Integer]
fractions = map fk [1..]

inverse :: Rational -> Rational
inverse x = denominator x % numerator x

e :: Int -> Rational
e acc = 2 + go (take (acc - 1) fractions)
  where
  go :: [Integer] -> Rational
  go [] = 0
  go [i] = 1 % i
  go (i:is) = inverse (fromIntegral i + go is)

digitSum :: Integer -> Integer
digitSum x = sum $ map (read . (:[])) (show x)
