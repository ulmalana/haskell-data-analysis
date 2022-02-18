module Chapter02 where

import Data.List
import Data.Either
import Text.CSV
import Database.HDBC
import Database.HDBC.Sqlite3

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


-- converts CSV to SQL DB
-- return "Sucsessful" if success
convertCSVToSQL :: String -> FilePath -> [String] -> CSV -> IO ()
convertCSVToSQL tableName outFile fields records = 
    -- make sure # column == # fields
    if nfieldsInFile == nfieldsInFields then do 
        conn <- connectSqlite3 outFile 

        -- create new table
        run conn createStatement []

        -- load CSV content to table
        stmt <- prepare conn insertStatement
        executeMany stmt (tail (filter (\record -> 
            nfieldsInFile == length record) 
            sqlRecords))

        -- commit changes and close connection
        commit conn
        disconnect conn

        putStrLn "Successful"
    else 
        putStrLn "The number of input fields does not match the csv."
    where
        nfieldsInFile = length $ head records
        nfieldsInFields = length fields
        createStatement = "CREATE TABLE " ++
                            tableName ++ 
                            " (" ++ (intercalate ", " fields) ++")"
        insertStatement = "INSERT INTO " ++
                            tableName ++ " VALUES (" ++
                            (intercalate ", " (replicate nfieldsInFields "?")) ++ ")"
        sqlRecords = map (\record -> map (\element -> toSql element) record) records 


-- convert CSV file to SQL DB
-- test with: 
-- ghci> convertCSVFileToSQL "all_week.csv" "earthquakes.sql" "oneWeek" ["time
--        TEXT", "latitude REAL", "longitude REAL", "depth REAL", "mag REAL",
--        "magType TEXT", "nst INTEGER", "gap REAL", "dmin REAL", "rms REAL", "net
--         REAL", "id TEXT", "updated TEXT", "place TEXT", "type TEXT" horizontalError REAL", 
--         "depthError REAL", "magError REAL", "magNst INTEGER", "status TEXT", "locationSource TEXT", "magSource TEXT"]

convertCSVFileToSQL :: String -> String -> String -> [String] -> IO ()
convertCSVFileToSQL inFile outFile tableName fields = do 
    input <- readFile inFile 
    let records = parseCSV inFile input

    either handleCSVError convertTool records
  where
    convertTool = convertCSVToSQL tableName outFile fields
    handleCSVError csv = putStrLn "This does not appear to be a CSV file."