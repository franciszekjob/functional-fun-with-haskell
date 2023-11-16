import Data.Char
isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: ([Int],Int) -> Int
getElemAtIdx (arr,idx) = head (drop idx arr)

capitalize :: [Char] -> [Char]
capitalize w = [toUpper (head w)] ++ tail w -- capitalize "ala" = "Ala"

pitagoreans3 = [(a,b,c) | a<-[1..10],b<-[1..10],c<-[1..10], a^2 + b^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

-- more effective, limit do sqrt(n)
isPrime2 :: Integral t => t -> Bool
isPrime2 n = [i | i <- [2..isqrt n], n `mod` i == 0] == [] where isqrt = floor . sqrt . fromIntegral

