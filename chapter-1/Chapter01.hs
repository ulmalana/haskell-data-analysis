module Chapter01 where

import Data.List

median :: [Double] -> Double
median [] = 0
median xs = if oddInLength 
            then midValue
            else (midValue + pre_midVal) / 2
    where
        sortedList = sort xs
        oddInLength = 1 == mod (genericLength xs) 2
        middle = floor $ (genericLength xs) / 2
        midValue = genericIndex sortedList middle
        pre_midVal = genericIndex sortedList (middle-1)

vowelIndices :: String -> [Integer]
vowelIndices word = map fst $ 
                    filter (\(_, letter) -> letter `elem` "aiueoAIUEO") $ 
                    zip [1..] word