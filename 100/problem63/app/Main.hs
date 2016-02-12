module Main where

import Euler

main :: IO ()
main = print $
  sum $ takeWhile (>0) $ map (length.nDigitNthPowers) [1..]
