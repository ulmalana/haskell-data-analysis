import Text.Regex.Posix ((=~))
import System.Environment (getArgs)

myGrep :: String -> String -> IO ()
myGrep myRegex fname = do 
    fileSlurp <- readFile fname 
    mapM_ putStrLn $ filter (=~ myRegex) (lines fileSlurp)

main :: IO ()
main = do 
    (myRegex:fnames) <- getArgs
    mapM_ (\file -> myGrep myRegex file) fnames