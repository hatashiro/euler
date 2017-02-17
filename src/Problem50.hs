module Problem50 where

isPrime :: Int -> Bool
isPrime n = not (any divisible [2..r])
  where r = floor $ sqrt (fromIntegral n)
        divisible m = n `mod` m == 0

primes :: [Int]
primes = filter isPrime [2..]

consecutivePrimeSum :: Int -> Int -> Int
consecutivePrimeSum from len = sum (take len $ drop from primes)

longestConsecutivePrimeSum :: Int -> (Int, Int)
longestConsecutivePrimeSum lt = helper lt 0 (0, 0)
  where
    helper lt idx previousMaximum =
      let maxLen = fst previousMaximum
          basePrime = consecutivePrimeSum idx maxLen in
      if basePrime > lt
         then (0, 0)
         else
           let current = takeWhile ((<=lt).snd) [(len, val) | len <- [maxLen..],
                                                              let val = consecutivePrimeSum idx len,
                                                              isPrime val]
               currentMaximum = if null current
                                   then previousMaximum
                                   else max (maximum current) previousMaximum
               next = helper lt (idx+1) currentMaximum in
           max currentMaximum next

main :: IO ()
main = print (longestConsecutivePrimeSum 1000000)
