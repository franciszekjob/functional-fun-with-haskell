fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then 1
    else fib (n-2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1
prod' (x:arr) = x * (prod' arr)

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:arr) = 1 + length' arr

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' _ [] = False
elem' e (x:arr) = e == x || elem' e arr

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:arr) = (x*2) : doubleAll arr

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:arr) = (x^2) : squareAll arr

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:arr) = if x `mod` 2 == 0 then x : selectEven arr else selectEven arr

prod'2 :: Num a => [a] -> a
prod'2 = loop 1
    where loop acc []      = acc
          loop acc (x:arr) = loop (x * acc) arr

length'2 :: Num a => [a] -> a
length'2 = loop 0
    where loop acc []      = acc
          loop acc (x:arr) = loop (1 + acc) arr

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

