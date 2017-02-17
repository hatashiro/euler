module Problem29 where

import Data.List

removeDuplication :: (Ord a) => [a] -> [a]
removeDuplication = map head . group . sort

main :: IO ()
main = putStrLn.show $
  length $ removeDuplication [a ^ b | a <- [2..100], b <- [2..100]]
