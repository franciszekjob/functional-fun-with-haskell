not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Correct answer" = True
isItTheAnswer _                = False

or' :: (Bool, Bool) -> Bool
or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False

and' :: (Bool, Bool) -> Bool
and' (a,b) = case (a,b) of
    (True,True) -> True
    _ -> False

-- roots for quadratic equations
roots :: (Double, Double, Double) -> (Double,Double)
roots (a,b,c) = ((-b-d)/(2*a),(-b+d)/(2*a))
    where d = sqrt(b*b - (4*a*c))

roots2 :: Double -> Double -> Double -> (Double,Double)
roots2 a b c = 
    let deltaSqrt = sqrt(b^2 - (4*a*c))
        e = 2*a
    in ((-b-deltaSqrt)/e, (-b+deltaSqrt)/e)

vec3DLength :: (Double, Double, Double) -> Double
vec3DLength (x,y,z) = 
    let length = sqrt(x^2 + y^2 + z^2)
    in length

subtract' a b = a - b