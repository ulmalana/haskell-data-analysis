module Chapter08 where

import Numeric.LinearAlgebra.Data
--import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.HMatrix
import Data.List as L 

import Chapter04
import Chapter06
import Chapter07

stopWords :: [String]
stopWords = ["a", "about", "above", "above", "across", "after",
    "afterwards", "again", "against", "all", "almost", "alone", "along",
    "already", "also", "although", "always", "am", "among", "amongst",
    "amoungst", "amount", "an", "and", "another", "any", "anyhow",
    "anyone", "anything", "anyway", "anywhere", "are", "around", "as",
    "at", "back", "be", "became", "because", "become", "becomes",
    "becoming", "been", "before", "beforehand", "behind", "being",
    "below", "beside", "besides", "between", "beyond", "bill", "both",
    "bottom", "but", "by", "call", "can", "cannot", "cant", "co", "con",
    "could", "couldnt", "cry", "de", "describe", "detail", "do", "done",
    "dont", "down", "due", "during", "each", "eg", "eight", "either",
    "eleven", "else", "elsewhere", "empty", "enough", "etc", "even",
    "ever", "every", "everyone", "everything", "everywhere", "except",
    "few", "fifteen", "fifty", "fill", "find", "fire", "first", "five",
    "for", "former", "formerly", "forty", "found", "four", "from",
    "front", "full", "further", "get", "give", "go", "got", "had", "has",
    "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby",
    "herein", "hereupon", "hers", "herself", "him", "himself", "his",
    "how", "however", "hundred", "i", "ie", "if", "im", "in", "inc",
    "indeed", "interest", "into", "is", "it", "its", "itself", "just",
    "keep", "last", "latter", "latterly", "least", "less", "ltd", "made",
    "many", "may", "me", "meanwhile", "might", "mill", "mine", "more",
    "moreover", "most", "mostly", "move", "much", "must", "my", "myself",
    "name", "namely", "neither", "need", "never", "nevertheless",
    "next", "nine", "no", "nobody", "none", "noone", "nor", "not",
    "nothing", "now", "nowhere", "of", "off", "often", "on", "once",
    "one", "only", "onto", "or", "other", "others", "otherwise", "our",
    "ours", "ourselves", "out", "over", "own", "part", "per", "perhaps",
    "please", "put", "rather", "re", "same", "see", "seem", "seemed",
    "seeming", "seems", "serious", "several", "she", "should", "show",
    "side", "since", "sincere", "six", "sixty", "so", "some", "somehow",
    "someone", "something", "sometime", "sometimes", "somewhere", "still",
    "such", "system", "take", "ten", "than", "that", "the", "their",
    "them", "themselves", "then", "thence", "there", "thereafter",
    "thereby", "therefore", "therein", "thereupon", "these", "they",
    "thick", "thin", "third", "this", "those", "though", "three",
    "through", "throughout", "thru", "thus", "to", "together", "too",
    "top", "toward", "towards", "twelve", "twenty", "two", "un", "under",
    "until", "up", "upon", "us", "very", "via", "want", "was", "we",
    "well", "were", "what", "whatever", "when", "whence", "whenever",
    "where", "whereafter", "whereas", "whereby", "wherein", "whereupon",
    "wherever", "whether", "which", "while", "whither", "who", "whoever",
    "whole", "whom", "whose", "why", "will", "with", "within", "without",
    "would", "yet", "you", "your", "youre", "yours", "yourself",
    "yourselves", "rt", "mt", "u"]

principalComponentAnalysis :: [[Double]] -> Integer -> Matrix Double
principalComponentAnalysis records top =
    tr $ mul (tr topOrderedEvectors) (tr featureVectors)
  where featureVectors = fromLists records
        (_, covMatrix) = meanCov featureVectors
        (_, evectors) = eigSH covMatrix
        topOrderedEvectors = takeColumns (fromInteger top) evectors

euclidianDistanceSqrdFromRow :: Matrix Double -> Integer -> Vector Double
euclidianDistanceSqrdFromRow records row = 
    sumOfSquaresVector
  where d = cols records
        copyMatrix = repmat (subMatrix (fromIntegral row, 0) (1,d) records) (rows records) 1
        diffMatrix = records - copyMatrix
        diffSqrdMatrix = diffMatrix * diffMatrix
        sumOfSquaresVector = sum $ toColumns diffSqrdMatrix

order :: Eq a => [a] -> [a] -> [Integer]
order unsorted nubSorted = 
    concatMap 
        (\x -> L.map toInteger $ L.elemIndices x unsorted)
        nubSorted

findClosestFeaturesToRow :: Matrix Double -> Integer -> Integer -> [Integer]
findClosestFeaturesToRow records row knn =
    take (fromIntegral knn) orderOfFeatures
  where sumOfSquares = toList $ euclidianDistanceSqrdFromRow records row 
        orderOfFeatures = order sumOfSquares . nub $ sort sumOfSquares