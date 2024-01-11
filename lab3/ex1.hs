-- let f1 = \x -> x - 2
-- let f2 = \(x,y) -> sqrt(x^2 + y^2)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' = sum' . map(^2)

double :: Num a => a -> a
double x = x * 2

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f (x:xs) = f x + sumWith f xs
sumWith f [] = 0
sumCub :: [Integer] -> Integer
sumCub = sumWith(\num -> num^3)