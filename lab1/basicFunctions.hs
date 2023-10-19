sqr :: Double -> Double
sqr x = x * x

vector2DLength :: (Double,Double) -> Double 
vector2DLength (x,y) = sqrt(x^2 + y^2)

vector3DLength :: (Double, Double, Double) -> Double
vector3DLength (x,y,z) = sqrt(x^2 + y^2 + z^2)

maxVal :: (Double,Double) -> Double
maxVal (a,b) = if a > b
    then a
    else b

sgn :: Int -> Int
sgn x = if x > 0
    then 1
    else if x == 0
        then 0
        else -1

absVal :: Int -> Int
absVal x = if x < 0
    then -x
    else x

min2Int :: (Int,Int) -> Int
min2Int (a,b) = if a < b then a else b

min3Int :: (Int,Int,Int) -> Int
min3Int (a, b, c) = min2Int(a, min2Int(b,c))

toUpperChar :: (Char) -> Char
toUpperChar c = if c >= 'a'  && c <= 'z' then toEnum (fromEnum c - 32) else c

toUpperString::(String) -> String
toUpperString str = map toUpperChar str