module Chapter03 where

import Text.CSV
import Data.List
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)

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