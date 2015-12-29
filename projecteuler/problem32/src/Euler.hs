module Euler where

import Data.Char
import Data.List

digits :: Int -> [Int]
digits n = map digitToInt $ show n

consistOf :: Int -> [Int] -> Bool
consistOf n xs = sort (digits n) == sort xs

canDigitMul :: Int -> Int -> Int -> Bool
canDigitMul x y z =
  x + y == z + 1 || x + y == z

multipliableDigitsInSum :: Int -> [(Int, Int)]
multipliableDigitsInSum s =
  [(x, y) | x <- [1..(s-2)], y <- [x..(s-x-1)], canDigitMul x y (s-x-y)]

removeElement :: Eq a => a -> [a] -> [a]
removeElement a xs =
  case elemIndex a xs of
    Just idx -> fst split ++ (tail.snd) split where split = splitAt idx xs
    Nothing  -> xs

integersFromListWithDigit :: Int -> [Int] -> [Int]
integersFromListWithDigit 1 xs = xs
integersFromListWithDigit digit xs =
  [x * 10^(digit-1) + rest | x <- xs,
                             rest <- integersFromListWithDigit (digit-1) (removeElement x xs)]
