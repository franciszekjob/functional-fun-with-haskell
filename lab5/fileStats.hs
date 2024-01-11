import System.Environment
import System.IO.Error
import Control.Exception

linesCount :: Int
linesCount = 0

charsCount :: Int
charsCount = 0

riskyAction :: IO ()
riskyAction = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn contents
    let linesCount' = linesCount + length (lines contents)
        charsCount' = charsCount + length contents
    putStrLn $ "Lines: " ++ show linesCount'
    putStrLn $ "Characters: " ++ show charsCount'

exHdlr :: IOError -> IO ()
exHdlr = \ex -> if isDoesNotExistError ex
                then putStrLn "The file doesn't exist!"
                else ioError ex

main :: IO ()
main = do
    result <- try riskyAction
    case result of
        Left e -> exHdlr e
        Right _ -> putStrLn "Action completed successfully."
