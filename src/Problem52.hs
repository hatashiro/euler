module Problem52 where

import Data.Digits
import Data.List

includesSameDigits :: Int -> Int -> Bool
includesSameDigits a b = sort aDigits == sort bDigits
  where aDigits = digits 10 a
        bDigits = digits 10 b

permutedMultiples :: Int -> Int -> Bool
permutedMultiples n x = all permutedMultiple [2..n]
  where permutedMultiple multiplier = includesSameDigits x (multiplier * x)

main :: IO ()
main = print $
  find (permutedMultiples 6) [2..]
