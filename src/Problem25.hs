module Problem25 where

fibo :: Int -> Integer
fibo 1 = 1
fibo 2 = 1
fibo n = (memoFibo (n-1)) + (memoFibo (n-2))

memoFibo :: Int -> Integer
memoFibo = ((map fibo [0..]) !!)

digit :: Integer -> Int
digit 0 = 0
digit n = 1 + (digit (n `div` 10))

firstNDigitFibo :: Int -> Int
firstNDigitFibo n = firstNDigitFiboWithIdx 1 n

firstNDigitFiboWithIdx :: Int -> Int -> Int
firstNDigitFiboWithIdx idx targetDigit =
  if (digit.memoFibo $ idx) == targetDigit
    then idx
    else firstNDigitFiboWithIdx (idx + 1) targetDigit

main :: IO ()
main = putStrLn . show $ firstNDigitFibo 1000
