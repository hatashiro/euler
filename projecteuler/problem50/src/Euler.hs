module Euler where

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor $ sqrt (fromIntegral n)
        divisible m = n `mod` m == 0

primes :: [Int]
primes = filter isPrime [2..]

consecutivePrimeSum :: Int -> Int -> Int
consecutivePrimeSum from len = sum (take len $ drop from primes)

longestConsecutivePrimeSum :: Int -> (Int, Int)
longestConsecutivePrimeSum lt = helper lt 0
  where
    helper lt idx =
      let basePrime = consecutivePrimeSum idx 1 in
      if basePrime > lt
         then (0, 0)
         else
           let current = maximum (filter (isPrime.snd) (takeWhile ((<=lt).snd) (map (\x -> (x, consecutivePrimeSum idx x)) [1..])))
               next = helper lt (idx+1) in
           max current next
