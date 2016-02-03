module Prime where

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = not (any divisible [2..r])
  where r = floor (sqrt $ fromIntegral n)
        divisible r = n `mod` r == 0

