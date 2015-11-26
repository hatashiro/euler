module Euler
( parseNames
, charValue
, stringValue
, namesScore
) where

import Data.Char
import Data.List
import Data.String.Utils
import System.IO

parseNames :: String -> [String]
parseNames str = map (replace "\"" "") $ split "," str

charValue :: Char -> Integer
charValue c = toInteger $ (ord c) - (ord 'A') + 1

stringValue :: String -> Integer
stringValue str = foldl1 (+) $ map charValue str

namesScore :: [String] -> Integer
namesScore names =
  let sortedNames = sort names
      sortedNamesWithOrdering = zip [1..] sortedNames
  in
      foldl1 (+) $ map (\x -> (fst x) * (stringValue (snd x))) sortedNamesWithOrdering
