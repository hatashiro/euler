module Euler where

import Data.Char
import Data.List.Split

parseWords :: String -> [String]
parseWords str = map trimDoubleQuotation tokens
  where tokens = splitOn "," str
        trimDoubleQuotation = init.tail

getWordValue :: String -> Int
getWordValue = foldl (\s x -> s + (ord x - 64)) 0

triangleNumber :: Int -> Int
triangleNumber = (map triangleNumber' [0..] !!)
  where triangleNumber' 1 = 1
        triangleNumber' n = triangleNumber' (n-1) + n

isTriangleNumber :: Int -> Bool
isTriangleNumber n = isTriangleNumber' n 1
  where
    isTriangleNumber' n i =
      let ith = triangleNumber i in
      n == ith || (ith < n && isTriangleNumber' n (i+1))
