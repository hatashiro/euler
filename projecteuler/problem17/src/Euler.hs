module Euler
( numberLetterCount
) where

hundredToString :: Int -> [Char]
hundredToString n
  | n == 0 = ""
  | otherwise = numberToString n ++ "hundred"

andOrEmpty :: Int -> Int -> Int -> [Char]
andOrEmpty _ 0 0 = ""
andOrEmpty 0 _ _ = ""
andOrEmpty _ _ _ = "and"

tenAndOneToString :: Int -> Int -> [Char]
tenAndOneToString 2 0 = "twenty"
tenAndOneToString 3 0 = "thirty"
tenAndOneToString 4 0 = "forty"
tenAndOneToString 5 0 = "fifty"
tenAndOneToString 6 0 = "sixty"
tenAndOneToString 7 0 = "seventy"
tenAndOneToString 8 0 = "eighty"
tenAndOneToString 9 0 = "ninety"
tenAndOneToString t o
  | t == 0 = numberToString o
  | t == 1 = numberToString $ t * 10 + o
  | otherwise = tenAndOneToString t 0 ++ numberToString o

numberToString :: Int -> [Char]
numberToString 0 = ""
numberToString 1 = "one"
numberToString 2 = "two"
numberToString 3 = "three"
numberToString 4 = "four"
numberToString 5 = "five"
numberToString 6 = "six"
numberToString 7 = "seven"
numberToString 8 = "eight"
numberToString 9 = "nine"
numberToString 10 = "ten"
numberToString 11 = "eleven"
numberToString 12 = "twelve"
numberToString 13 = "thirteen"
numberToString 14 = "fourteen"
numberToString 15 = "fifteen"
numberToString 16 = "sixteen"
numberToString 17 = "seventeen"
numberToString 18 = "eighteen"
numberToString 19 = "nineteen"
numberToString 1000 = "onethousand"
numberToString n =
  (hundredToString h) ++ (andOrEmpty h t o) ++ (tenAndOneToString t o)
  where h = n `div` 100
        t = (n `mod` 100) `div` 10
        o = (n `mod` 10)

numberLetterCount :: Int -> Int
numberLetterCount num =
  length $ numberToString num
