echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3 = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ l1 ++ " " ++ l2

doDialog = do
    putStr "What is your happy number? "
    line <- getLine
    let number = read line :: Int
    if number == 7
    then putStrLn "Ah, lucky 7!"
    else if odd number
        then putStrLn "Odd number! That's most people's choice..."
        else putStrLn "Hm, even number? Unusual!"
