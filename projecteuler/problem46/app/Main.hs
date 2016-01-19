module Main where

import Euler
import Data.List

main :: IO ()
main = print $
  find (not.goldbachConjecture) oddComps
