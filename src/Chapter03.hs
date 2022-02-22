module Chapter03 where

import Text.CSV
import Data.List
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)
import Chapter02

-- check the length of field in each record
countFieldsInEachRecord :: CSV -> [Integer]
countFieldsInEachRecord csv = map genericLength (init csv)


-- compare the field length of record with the actual length.
-- if different, print them.
lineNumberWithIncorrectCount :: CSV -> [(Integer, Integer)]
lineNumberWithIncorrectCount (fields:csv) = filter 
    (\(_, thisCount) -> thisCount /= nfields)
    lineNoCountPairs
  where
    nfields = genericLength fields 
    count = countFieldsInEachRecord csv
    lineNoCountPairs = zip [1..] count

identifyMatchingFields :: (String -> Bool)
                        -> [String]
                        -> [String]
                        -> Integer
                        -> [(String, String, String)]
identifyMatchingFields myStringCmpFunc record headings idColumnIndex = 
   filter 
      (\(_, _, field) -> myStringCmpFunc field)
      keyvalue
    where
      nfields = length headings
      keyvalue = zip3 (replicate nfields (genericIndex record idColumnIndex))
                 headings
                 record

identifyInCSV :: (String -> Bool) -> CSV -> String ->
                    Either String [(String, String, String)]
identifyInCSV myFieldFunc csv idColumn =
    either
        (Left)
        (\ci -> Right $ concatMap
            (\record ->
                identifyMatchingFields myFieldFunc record (head csv) ci
                )
                (tail csv))
        columnIndex
    where
        headings = head csv 
        columnIndex = getColumnInCSV csv idColumn

identifyInCSVFile :: (String -> Bool) -> String -> String -> IO (Either String [(String, String, String)])
identifyInCSVFile myStringCmpFunc inFileName idColumn = do
    records <- parseCSVFromFile inFileName
    return $ either
                (\err ->
                 Left "This does not appear to be a CSV file")
                (\csv ->
                    identifyInCSV myStringCmpFunc (init csv) idColumn
                )
                records

identifyInCSVFileFromColumn :: (String -> Bool)
                            -> String 
                            -> String
                            -> String
                            -> IO (Either String [(String, String, String)])
identifyInCSVFileFromColumn 
    myRegexFunc inFileName idColumn desiredHeading = do 
        allFields <- identifyInCSVFile
                        myRegexFunc inFileName idColumn
        return $ either
                    (Left)
                    (\af -> Right $
                        filter (\(_, heading, _) ->
                            heading == desiredHeading
                            )
                        af
                    )
                    allFields                                            