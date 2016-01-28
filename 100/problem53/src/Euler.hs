module Euler where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

combination :: Integer -> Integer -> Integer
combination n r =
  if fromIntegral r < (fromIntegral n / 2) then combination' n r
                 else combination' n (n-r)
    where
      combination' n r = product [n, (n-1) .. (n-r+1)] `div` fac r
