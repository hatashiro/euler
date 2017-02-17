module Problem27 where

import Data.List

quadratic :: Int -> Int -> Int -> Int
quadratic a b n = n * n + a * n + b

isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | otherwise = isPrimeMemo n

isPrimeMemo :: Int -> Bool
isPrimeMemo = ((map isPrimeInternal [0..]) !!)

isPrimeInternal :: Int -> Bool
isPrimeInternal 2 = True
isPrimeInternal n = (fermatTest n) && (detTest n)

fermatTest :: Int -> Bool
fermatTest n = (2 ^ (n-1) `mod` (toInteger n)) == 1

detTest :: Int -> Bool
detTest n = not $ any (\x -> n `mod` x == 0) [2..root]
  where root = floor $ sqrt $ fromIntegral n

primeSeq :: (Int -> Int) -> Int
primeSeq quad =
  case findIndex (not . isPrime . quad) [0..] of
    Just n  -> n
    Nothing -> -1

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
