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