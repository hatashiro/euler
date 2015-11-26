module Euler
( parseNames
, names
) where

import Data.String.Utils
import System.IO

parseNames :: String -> [String]
parseNames str = map (replace "\"" "") $ split "," str

names :: [String]
names = do
  {- contents <- readFile "resource/names.txt" ReadMode-}
  {- splitAndParse contents-}
  []
