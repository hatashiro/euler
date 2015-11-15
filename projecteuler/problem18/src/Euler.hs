module Euler
( getVal
, getRow
, Node(Node, NullNode)
, createNode
, maxSum
, tree
) where

import Control.Exception
import Data.List.Split

data Node = Node Int Node Node | NullNode deriving (Show, Eq)

tree :: Node
tree = createNode 0 0

maxSum :: Node -> Int
maxSum NullNode = 0
maxSum (Node val c1 c2) =
  if c1Sum > c2Sum
    then val + c1Sum
    else val + c2Sum
  where c1Sum = maxSum c1
        c2Sum = maxSum c2

createNode :: Int -> Int -> Node
createNode row col =
  case val of
    (Just val) -> Node val (createNode (row+1) col) (createNode (row+1) (col+1))
    (Nothing) -> NullNode
  where val = getVal row col

getRow :: Int -> Maybe [Char]
getRow rowNum =
  let rows = splitOn "\n" treeStr
  in
    if (rowNum >= (length rows))
      then Nothing
      else (Just $ rows !! rowNum)

getVal :: Int -> Int -> Maybe Int
getVal rowNum colNum =
  case maybeRow of
    (Just row) ->
      let cols = splitOn " " row
      in
        if (colNum >= (length cols))
          then Nothing
          else (Just $ (read $ cols !! colNum :: Int))
    (Nothing) -> Nothing
  where maybeRow = getRow rowNum

treeStr :: [Char]
treeStr = "75\n\
          \95 64\n\
          \17 47 82\n\
          \18 35 87 10\n\
          \20 04 82 47 65\n\
          \19 01 23 75 03 34\n\
          \88 02 77 73 07 63 67\n\
          \99 65 04 28 06 16 70 92\n\
          \41 41 26 56 83 40 80 70 33\n\
          \41 48 72 33 47 32 37 16 94 29\n\
          \53 71 44 65 25 43 91 52 97 51 14\n\
          \70 11 33 28 77 73 17 78 39 68 17 57\n\
          \91 71 52 38 17 14 91 43 58 50 27 29 48\n\
          \63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
          \04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
