module Prime where

isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [x | x <- [2..(isqrt n)], n `mod` x == 0]
