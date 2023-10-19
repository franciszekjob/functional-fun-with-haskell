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
-- roots :: (Double, Double, Double) -> (Double,Double)
-- roots (a,b,c) = ((-b-d)/(2*a),(-b+d)/(2*a))
--     where d = sqrt(b*b - (4*a*c))

