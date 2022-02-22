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
queryDatabase dbFile sqlQuery = do
    conn <- connectSqlite3 dbFile
    result <- quickQuery' conn sqlQuery []
    disconnect conn
    return  result

pullStockClosingPrices :: String -> String -> IO [(Double, Double)]
pullStockClosingPrices dbFile db = do
    sqlResult <- queryDatabase dbFile ("SELECT rowid, adjclose FROM " ++ db)
    
    -- this line is different from the books (without reverse)
    -- since the data is already in chronological order
    return $ zip (readDoubleColumn sqlResult 0) (readDoubleColumn sqlResult 1)

percentChange :: Double -> Double -> Double
percentChange value first = 100.0 * (value - first) / first

applyPercentChangeToData :: [(Double, Double)] -> [(Double, Double)]
applyPercentChangeToData dataset = zip indices scaledData
    where 
        (_, first) = last dataset
        indices = reverse [1.0..(genericLength dataset)]
        scaledData = map (\(_, value) -> percentChange value first) dataset

movingAverage :: [Double] -> Integer -> [Double]
movingAverage values window 
  | window >= genericLength values = [ avg values ]
  | otherwise = avg (genericTake window values) : (movingAverage (tail values) window)

applyMovingAverageToData :: [(Double, Double)] -> Integer -> [(Double, Double)]
applyMovingAverageToData dataset window = zip [fromIntegral window..] $
                                            movingAverage (map snd (reverse dataset))
                                            window

pullLatitudeLongitude :: String -> String -> IO [(Double, Double)]
pullLatitudeLongitude dbFile db = do 
    sqlResult <- queryDatabase dbFile ("SELECT latitude, longitude FROM " ++ db)
    return $ zip (readDoubleColumn sqlResult 1) (readDoubleColumn sqlResult 0)