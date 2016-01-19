module Main where

import Control.Monad (join)
import Data.Digits
import Euler

main :: IO ()
main = print $
  unDigits 10 $ lastDigits 10 $ sum (map (join (^)) [1..1000])
