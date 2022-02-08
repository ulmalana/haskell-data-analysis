module Chapter02 where

import Data.List
import Data.Either
import Text.CSV

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / fromIntegral (length xs)

getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv columnName =
    case lookupResponse of 
        Nothing -> Left "Column doesnt exist"
        Just x -> Right (fromIntegral x)
  where lookupResponse = findIndex (== columnName) (head csv)

applyToColumnInCSV :: ([String] -> b) -> CSV -> String -> Either String b
applyToColumnInCSV func csv column = either
                    (Left)
                    (Right . func . elements)
                columnIndex
                where
                    columnIndex = getColumnInCSV csv column
                    nfieldsInFile = length $ head csv
                    records = tail $
                        filter (\record -> nfieldsInFile == length record) csv
                    elements ci = map (\record -> genericIndex record ci) records