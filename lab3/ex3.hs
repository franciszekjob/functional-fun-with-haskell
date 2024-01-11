import Data.List (sort)
funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

let f = (\(x,y) -> let (c,d) = (x^2+y, y^2+x) in (c+d) * (c*d)*(1+c^2+d^2)) :: (Int, Int) -> Int

sortDesc :: Ord a => [a] -> [a]
-- sortDesc xs = (reverse . sort) xs
sortDesc = (reverse . sort)  -- point free

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 0 = x : onlyOdd xs
    | otherwise      = onlyOdd xs