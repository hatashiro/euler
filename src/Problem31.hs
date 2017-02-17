module Problem31 where

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

knapsack :: [Int] -> Int -> Int
knapsack xs obj
  | obj < 0  = 0
  | obj == 0 = 1
  | otherwise = sum (map (\(idx, val) -> knapsack (drop idx xs) (obj - val)) (zip [0..] xs))

main :: IO ()
main = print (knapsack coins 200)
