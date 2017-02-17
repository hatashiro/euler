module Problem63 where

nthPowers :: (Integral a) => a -> [a]
nthPowers n = map (^n) [1..]

nDigitNthPowers :: (Integral a) => a -> [a]
nDigitNthPowers n = takeWhile (<= 10^n - 1) $ dropWhile (< 10^(n-1)) (nthPowers n)

main :: IO ()
main = print $
  sum $ takeWhile (>0) $ map (length.nDigitNthPowers) [1..]
