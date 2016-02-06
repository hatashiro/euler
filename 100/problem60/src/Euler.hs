module Euler where

import Data.Digits
import Data.List (find)
import Data.Maybe
import Data.Numbers.Primes (isPrime)
import Data.Set (Set, elems, insert, size, member, fromList)
import Data.Tuple (swap)

isPrimePair :: Int -> Int -> Bool
isPrimePair x y = isPrime (concatInt x y) && isPrime (concatInt y x)
  where concatInt x y = unDigits 10 (digits 10 x ++ digits 10 y)

filterJust :: [Maybe a] -> [a]
filterJust = foldl appendIfJust []
  where appendIfJust xs m = if isJust m then fromJust m : xs else xs

retrievePrimePairs :: Int -> [(Int, Int)]
retrievePrimePairs 2 = []
retrievePrimePairs 3 = []
retrievePrimePairs 5 = []
retrievePrimePairs 7 = []
retrievePrimePairs x = filterJust $ map (split ds) [1..(length ds - 1)]
  where
    ds = digits 10 x
    split xs idx =
      case splitAt idx xs of
        (as, 0:_) -> Nothing
        (as, bs ) -> let a = unDigits 10 as
                         b = unDigits 10 bs in
                     if isPrime a && isPrime b && isPrimePair a b && a /= b
                        then Just (a, b)
                        else Nothing

mergePrimeSet :: Set Int -> (Int, Int) -> Set Int
mergePrimeSet set pair = helper (helper set pair) (swap pair)
  where
    helper set (a, b) =
      if member a set && all (isPrimePair b) (elems set)
         then insert b set
         else set

primes :: [Int]
primes = [x | x <- [3..], isPrime x]

primePairSet :: Int -> Set Int
primePairSet n = helper n [] primes
  where
    helper n sets (p:primes) =
      fromMaybe (helper n mergedSets primes)
        (find (\set -> size set == n) mergedSets)
      where
        pairs = retrievePrimePairs p
        merge sets pair =
          let updatedSets = map (`mergePrimeSet` pair) sets in
          if updatedSets == sets
             then fromList [fst pair, snd pair] : updatedSets
             else updatedSets
        mergedSets = foldl merge sets pairs
