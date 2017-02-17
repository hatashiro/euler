module Problem28 where

getLayerCellCount :: Int -> Int
getLayerCellCount 1 = 1
getLayerCellCount n = 8 * (n - 1)

getFirstNum :: Int -> Int
getFirstNum 1 = 0
getFirstNum n = prevLayerLength ^ 2
  where prevLayerLength = ((getLayerCellCount (n-1)) `div` 4) + 1

getCornerSum :: Int -> Int
getCornerSum 1 = 1
getCornerSum n = first * 4 + add * 10
  where
    first = getFirstNum n
    add = (getLayerCellCount n) `div` 4

getDiagnoalSum :: Int -> Int
getDiagnoalSum dimension = sum $ map getCornerSum [1..n]
  where n = (dimension `div` 2) + 1

main :: IO ()
main = putStrLn . show $ getDiagnoalSum 1001
