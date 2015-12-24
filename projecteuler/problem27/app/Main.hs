module Main where

import Euler

main :: IO ()
main = do
  let quads = [(a, b, (quadratic a b)) | a <- [-999..999], b <- [-999..999]]
  putStrLn.show $ foldl (\(maxSeq, prod) (a, b, quad) ->
    let pSeq = primeSeq quad
    in
      if pSeq > maxSeq
        then (pSeq, a*b)
        else (maxSeq, prod)
    ) (0, 0) quads
