module Main where

import Lib

main :: IO ()
main = do
  title <- prompt "Please enter the title of today's work: "
  putStrLn $ "You did " ++ title
