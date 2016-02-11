module Euler where

import qualified Data.HashMap as M
import Data.List

cubes :: [Int]
cubes = map (^3) [1..]

getHashKey :: Int -> String
getHashKey n = sort (show n)

cubicPermutations :: Int -> Int
cubicPermutations n = cubicPermutations' n M.empty cubes
  where
    cubicPermutations' n hashMap (cube:cubes) =
      let key = getHashKey cube
          list = cube : M.findWithDefault [] key hashMap in
      if length list == n
         then minimum list
         else cubicPermutations' n (M.insert key list hashMap) cubes
