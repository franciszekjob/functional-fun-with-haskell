sgn :: Int -> Int
sgn n | n > 0 = 1
      | n < 0 = -1
      | n == 0 = 0

absInt :: Int -> Int
absInt n | n >= 0 = n
         | otherwise = -n


f2 n =  if n == 2 then 10 else if n == 1 then 3 else  1

