import System.Environment (getArgs)
import Chapter01

main :: IO ()
main = do 
    val <- getArgs
    print . median $ map read val