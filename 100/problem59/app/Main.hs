module Main where

import Data.Char
import Data.List
import Data.Maybe
import Euler

main :: IO ()
main = do
  raw <- readFile "p059_cipher.txt"
  let codes = parseIntList raw
  print $
    case find isJust $ map (decrypt codes) possiblePasswords of
      Just (Just str) -> sum (map ord str)
      Nothing  -> -1
