module Euler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.List.Split

toVal :: Char -> Int
toVal '2' = 2
toVal '3' = 3
toVal '4' = 4
toVal '5' = 5
toVal '6' = 6
toVal '7' = 7
toVal '8' = 8
toVal '9' = 9
toVal 'T' = 10
toVal 'J' = 11
toVal 'Q' = 12
toVal 'K' = 13
toVal 'A' = 14
toVal 'S' = 3
toVal 'H' = 2
toVal 'D' = 1
toVal 'C' = 0

mapTuple = join (***)

type Card = (Int, Int)

parseCard :: String -> Card
parseCard [number, symbol] = (toVal number, toVal symbol)

numbers :: [Card] -> [Int]
numbers = map fst

symbols :: [Card] -> [Int]
symbols = map snd

parseHands :: String -> ([Card], [Card])
parseHands raw = mapTuple sort $ splitAt 5 (map parseCard tokens)
  where tokens = splitOn " " raw

same :: (Eq a) => [a] -> Bool
same (x:xs) = all (==x) xs

type Combination = [Card] -> Maybe Card

and :: Combination -> Combination -> Combination
and c1 c2 xs =
  case c1 xs of
    Just _  -> c2 xs
    Nothing -> Nothing

flush :: Combination
flush cards = if same (symbols cards) then Just (last cards) else Nothing

straight :: Combination
straight cards =
  let (x:xs) = numbers cards in
  if isStraight x xs then Just (last cards)
                     else Nothing
    where
      isStraight _    []     = True
      isStraight cur (x:xs) = (cur+1 == x || cur+9 == x) && isStraight x xs

straightFlush :: Combination
straightFlush = straight `Euler.and` flush

royalFlush :: Combination
royalFlush cards =
  if numbers cards == [10, 11, 12, 13, 14]
     then flush cards
     else Nothing

fourOfAKind :: Combination
fourOfAKind cards = fmap last (find (\g -> length g == 4) groups)
  where groups = groupBy ((==) `on` fst) cards

threeOfAKind :: Combination
threeOfAKind cards = fmap last (find (\g -> length g == 3) groups)
  where groups = groupBy ((==) `on` fst) cards

type MultipleCombination = [Card] -> Maybe [Card]

twoPairs :: MultipleCombination
twoPairs cards =
  case filter (\g -> length g == 2) groups of
    [a, b] -> let al = last a
                  bl = last b in
              Just (if al > bl then [al, bl] else [bl, al])
    _      -> Nothing
  where groups = groupBy ((==) `on` fst) cards

highCard :: MultipleCombination
highCard cards =
  case map (!!0) singles of
    [] -> Nothing
    xs -> Just (sortBy (flip compare) xs)
  where
    groups = groupBy ((==) `on` fst) cards
    singles = filter (\g -> length g == 1) groups

onePair :: Combination
onePair cards =
  case filter (\g -> length g == 2) groups of
    [a] -> Just (last a)
    _   -> Nothing
  where groups = groupBy ((==) `on` fst) cards

fullHouse :: MultipleCombination
fullHouse cards =
  threeOfAKind cards >>= (\a -> fmap (\b -> [a, b]) (onePair cards))

evaluateHand :: [Card] -> (Maybe Int, Maybe Int, Maybe Int, Maybe [Int], Maybe Int, Maybe Int, Maybe Int, Maybe [Int], Maybe Int, Maybe [Int])
evaluateHand cards =
  (fst <$> royalFlush cards,
   fst <$> straightFlush cards,
   fst <$> fourOfAKind cards,
   map fst <$> fullHouse cards,
   fst <$> flush cards,
   fst <$> straight cards,
   fst <$> threeOfAKind cards,
   map fst <$> twoPairs cards,
   fst <$> onePair cards,
   map fst <$> highCard cards)
