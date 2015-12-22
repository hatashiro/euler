module Main where

import Euler

main :: IO ()
main = putStrLn.show $ foldl (\maxi n ->
  let len = (length $ getRecurringCycle n) in
  if (snd maxi) < len
    then (n, len)
    else maxi) (0, 0) [1..1000]
