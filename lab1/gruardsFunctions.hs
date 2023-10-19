absInt :: Int -> Int
absInt x | x >= 0 = x | otherwise = -x

min2Int :: (Int,Int) -> Int
min2Int (a,b) | a < b = a
              | otherwise = b

        