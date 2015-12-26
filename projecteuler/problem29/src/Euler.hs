module Euler
( removeDuplication
) where

import Data.List

removeDuplication :: (Ord a) => [a] -> [a]
removeDuplication = map head . group . sort
