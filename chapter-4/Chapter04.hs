module Chapter04 where

import Data.List 
import Database.HDBC.Sqlite3
import Database.HDBC
import Graphics.EasyPlot
import Chapter02

readIntegerColumn :: [[SqlValue]] -> Integer -> [Integer]
readIntegerColumn sqlResult index = map (\ row -> fromSql $ genericIndex row index :: Integer) sqlResult

readDoubleColumn :: [[SqlValue]] -> Integer -> [Double]
readDoubleColumn sqlResult index = map (\ row -> fromSql $ genericIndex row index :: Double) sqlResult

readStringColumn :: [[SqlValue]] -> Integer -> [String]
readStringColumn sqlResult index = map (\ row -> fromSql $ genericIndex row index :: String) sqlResult

queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
    conn <- connectSqlite3 databaseFile
    result <- quickQuery' conn sqlQuery []
    disconnect conn
    return  result

pullStockClosingPrices :: String -> String -> IO [(Double, Double)]
pullStockClosingPrices databaseFile database = do
    sqlResult <- queryDatabase databaseFile ("SELECT rowid, adjclose FROM " ++ database)
    -- this line is different from the books (without reverse)
    -- since the data is already in chronological order
    return $ zip (readDoubleColumn sqlResult 0) (readDoubleColumn sqlResult 1)