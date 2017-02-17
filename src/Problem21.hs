module Problem21 where

divisorSumWithDivisor :: Int -> Int -> Int
divisorSumWithDivisor n divisor
  | noMore = 0
  | n == 1 = 0
  | divisor == 1 = 1 + (divisorSumWithDivisor n 2)
  | otherwise =
      if n `mod` divisor /= 0
        then divisorSumWithDivisor n (divisor+1)
        else
          let dividend = n `div` divisor
          in
            if dividend == divisor
              then divisor + (divisorSumWithDivisor n (divisor+1))
              else divisor + dividend + (divisorSumWithDivisor n (divisor+1))
  where root = sqrt (fromIntegral n)
        noMore = (fromIntegral divisor) > root

divisorSum :: Int -> Int
divisorSum n = divisorSumWithDivisor n 1

d :: Int -> Int
d = (map divisorSum [0..] !!)

isAmicable :: Int -> Bool
isAmicable n = ddn == n && dn /= n
  where dn = d n
        ddn = d dn

main :: IO ()
main =
  putStrLn.show.sum $ map (\x -> if isAmicable x then x else 0) [1..9999]
