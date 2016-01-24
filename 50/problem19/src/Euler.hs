module Euler
( getDayOfWeek
, isLeapYear
, daysOfMonth
, daysOfYear
) where

getDayOfWeek :: Int -> Int -> Int -> Int
getDayOfWeek 1900 1 1 = 1
getDayOfWeek year 1 1 = (base + (foldl1 (+) $ map daysOfYear [1900..(year-1)])) `mod` 7
  where base = getDayOfWeek 1900 1 1
getDayOfWeek year 1 day = (day - 1 + (getDayOfWeek year 1 1)) `mod` 7
getDayOfWeek year month day = ((getDayOfWeek year 1 day) + (foldl1 (+) $ map (daysOfMonth year) [1..(month-1)])) `mod` 7

daysOfMonth :: Int -> Int -> Int
daysOfMonth _ 1 = 31
daysOfMonth year 2
  | isLeapYear(year) = 29
  | otherwise        = 28
daysOfMonth _ 3 = 31
daysOfMonth _ 4 = 30
daysOfMonth _ 5 = 31
daysOfMonth _ 6 = 30
daysOfMonth _ 7 = 31
daysOfMonth _ 8 = 31
daysOfMonth _ 9 = 30
daysOfMonth _ 10 = 31
daysOfMonth _ 11 = 30
daysOfMonth _ 12 = 31

daysOfYear :: Int -> Int
daysOfYear year
  | isLeapYear(year) = 366
  | otherwise        = 365

isLeapYear :: Int -> Bool
isLeapYear year =
  year `mod` 4 == 0 && (not (year `mod` 100 == 0) || year `mod` 400 == 0)
