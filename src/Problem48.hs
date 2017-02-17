module Problem48 where

import Control.Monad (join)
import Data.Digits

lastDigits :: Int -> Integer -> [Integer]
lastDigits n x = lastN n (digits 10 x)
  where lastN n xs = drop (length xs - n) xs

main :: IO ()
main = print $
  unDigits 10 $ lastDigits 10 $ sum (map (join (^)) [1..1000])
