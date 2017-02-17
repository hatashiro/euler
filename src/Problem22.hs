module Problem22 where

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

names :: IO [String]
names = do
  content <- readFile "resource/names.txt"
  return $ parseNames content

main :: IO ()
main = do
  names >>= putStrLn . show . namesScore
