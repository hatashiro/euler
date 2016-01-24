module Main where

import Euler

sunday :: Int -> Int -> Int -> Int
sunday year month day =
  if getDayOfWeek year month day == 0
    then 1
    else 0

isFirstSunday :: Int -> Int -> Int
isFirstSunday year month = sunday year month 1

firstSundaysInYear :: Int -> Int
firstSundaysInYear year = foldl1 (+) (map (isFirstSunday year) [1..12])

main :: IO ()
main =
  putStrLn $ show $ foldl1 (+) (map firstSundaysInYear [1901..2000])
