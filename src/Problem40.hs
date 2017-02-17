module Problem40 where

import Data.Digits

irrationalSequence :: [Int]
irrationalSequence = concatMap (digits 10) [1..]

main :: IO ()
main = print $
  product $ map (irrationalSequence !!) [0, 9, 99, 999, 9999, 99999, 999999]
