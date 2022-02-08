module Chapter02 where

import Data.List
import Data.Either
import Text.CSV

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / fromIntegral (length xs)

readColumn :: [String] -> [Double]
readColumn = map read

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
                                        records = tail $ filter (\record -> nfieldsInFile == length record) csv
                                        elements ci = map (\record -> genericIndex record ci) records

-- Opens a CSV file and applies a function to a column
-- Returns Either Error Message or the function result
applyToColumnInCSVFile :: ([String] -> b) -> FilePath -> String -> IO (Either String b)
applyToColumnInCSVFile func inFileName column = do
    -- Open and read the CSV file
    input <- readFile inFileName
    let records = parseCSV inFileName input
    -- Check to make sure this is a good csv file
    return $ either
        (handleCSVError)
        (\csv -> applyToColumnInCSV func csv column)
        records
    where
        handleCSVError csv = Left "This does not appear to be a CSV file."