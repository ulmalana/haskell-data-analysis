{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- Note: Reference on how to use Twitter API with Haskell
-- https://techracho.bpsinc.jp/jhonda/2017_12_18/49797


-- This code use Twitter API V1.1

-- HOW TO RUN
-- 
-- > :l Chapter07.hs
-- > createTweetsDatabase
-- > mapM_ (\x -> collectTweetsIntoDatabase) [1..180] 
--
-- > sqlTweets <- queryDatabase "tweets.sql" "SELECT message, language FROM
-- tweets"
-- > let tweets = zip (readStringColumn sqlTweets 0) (readStringColumn
-- sqlTweets 1)
--
-- CLEANING THE TWEET
-- > let freqTable = frequency tweets
-- > let uniqueTweets = HM.keys freqTable
-- > let cleanedTweets = zip (L.map (removePunctuation.clean.fst)
-- uniqueTweets) (L.map snd uniqueTweets)

-- CREATE FEATURE
-- > let languageFrequency = (frequency . L.map snd) cleanedTweets
-- > let allLanguages = HM.keys languageFrequency
-- > let wordFrequency = (frequency . concatMap words) (L.map fst
-- cleanedTweets)
-- > let wordFrequencyByLanguage = (HM.fromList . L.map (\language ->
-- (language, (frequency . concatMap words . L.map fst) (L.filter (\tweet ->
-- language == (snd tweet)) cleanedTweets)))) allLanguages

-- TESTING 
-- > probLanguageGivenWord "en" "house" languageFrequency wordFrequency
-- wordFrequencyByLanguage
-- > probLanguageGivenWord "en" "casa" languageFrequency wordFrequency
-- wordFrequencyByLanguage
-- > probLanguageGivenMessage "en" "my house is your house"
-- languageFrequency wordFrequencyByLanguage
-- > maxClassifier $ languageClassifierGivenMessage "the quick brown fox
-- jumps over the lazy dog" languageFrequency wordFrequencyByLanguage
-- > maxClassifier $ languageClassifierGivenMessage "feliz cumpleaños"
-- languageFrequency wordFrequencyByLanguage

module Chapter07 where

import Data.List as L 
import Data.Hashable
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC
import Control.Concurrent
import Data.Char
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics
import Data.Ord (comparing)

import Chapter02
import Chapter04

-- This code use Twitter API V1.1
myoauth :: OAuth
myoauth = newOAuth {
    oauthServerName = "api.twitter.com",
    oauthConsumerKey = "YOUR API/CONSUMER KEY",
    oauthConsumerSecret = "YOUR API/CONSUMER SECRET KEY"
}

mycred :: Credential
mycred = newCredential "YOUR ACCESS TOKEN" "YOUR ACCESS TOKEN SECRET"

data User2 = User2 {
    screen_name :: !String
} deriving (Show, Generic)

data Status = Status {
    text :: !String,
    lang :: !String,
    user :: !User2
} deriving (Show, Generic)

data Search = Search {
    statuses :: ![Status]
} deriving (Show, Generic)

instance FromJSON User2
instance ToJSON User2
instance FromJSON Status
instance ToJSON Status
instance FromJSON Search
instance ToJSON Search

twitterSearch :: String -> IO (Either String Search)
twitterSearch term = do 
    req <- parseRequest $ 
           "https://api.twitter.com/1.1/search/tweets.json?count=100&q=" ++
           term 
    -- res <- withManager $ \m -> do 
    --             signedreq <- signOAuth myoauth mycred req
    --             httpLbs signedreq m
    signedreq <- signOAuth myoauth mycred req
    manager <- newManager tlsManagerSettings
    response <- httpLbs signedreq manager

    return $ eitherDecode $ responseBody response

createTweetsDatabase :: IO ()
createTweetsDatabase = do 
    conn <- connectSqlite3 "tweets.sql"
    run conn createStatement []
    commit conn 
    disconnect conn 
    putStrLn "Successfully created database"
  where createStatement = "CREATE TABLE tweets (message TEXT, user TEXT, language TEXT)"

insertTweetsInDatabase :: [Status] -> IO ()
insertTweetsInDatabase tweets = do 
    conn <- connectSqlite3 "tweets.sql"
    stmt <- prepare conn insertStatement
    executeMany stmt sqlRecords
    commit conn 
    disconnect conn 
    putStrLn "Successfully inserted tweets to database"
  where insertStatement = "INSERT INTO tweets VALUES (?, ?, ?)"
        sqlRecords = L.map (\(Status message language (User2 user)) ->
                     [toSql message, toSql user, toSql language])
                     tweets

collectTweetIntoDatabase :: IO ()
collectTweetIntoDatabase = do 
    status <- twitterSearch "a"
    either 
        (putStrLn) 
        (\(Search statuses) -> insertTweetsInDatabase statuses)
        status 
    threadDelay 5000

frequency :: (Eq k, Data.Hashable.Hashable k, Integral v) => [k] -> HashMap k v 
frequency [] = HM.empty
frequency (x:xs) = HM.insertWith (+) x 1 (frequency xs)

-- removes replies, hashtags, links
clean :: String -> String
clean myString = unwords $ L.filter 
    (\myWord -> not (or 
                [ isInfixOf "@" myWord,
                  isInfixOf "#" myWord,
                  isInfixOf "http://" myWord]))
    (words myString)

removePunctuation :: String -> String
removePunctuation myString = [toLower c | c <- myString, or [isAlpha c, isSpace c]]

probLanguageGivenWord :: String
                      -> String
                      -> HashMap String Integer
                      -> HashMap String Integer
                      -> HashMap String (HashMap String Integer)
                      -> Double
probLanguageGivenWord 
    language 
    word
    wordLanguageFrequency
    wordFrequency
    wordFrequencyByLanguage = pLanguage * pWordGivenLanguage / pWord
  where countTweets = fromIntegral . sum $ elems wordLanguageFrequency 
        countAllWords = fromIntegral . sum $ elems wordFrequency
        countLanguage = fromIntegral $ lookupDefault 0 language wordLanguageFrequency
        countWordsUsedInLanguage = fromIntegral . sum . elems $
                                   wordFrequencyByLanguage !
                                   language
        countWord = fromIntegral $ lookupDefault 0 word wordFrequency
        countWordInLanguage = fromIntegral $ lookupDefault 0 word
                              (wordFrequencyByLanguage ! language)
        pLanguage = countLanguage / countTweets
        pWordGivenLanguage = countWordInLanguage / countWordsUsedInLanguage
        pWord = countWord / countAllWords

probLanguage :: String
              -> HashMap String Integer
              -> Double
probLanguage language languageFrequency =
    countLanguage / countTweets
  where countTweets = fromIntegral . sum $ elems languageFrequency
        countLanguage = fromIntegral $ lookupDefault 0 language languageFrequency


probWordGivenLanguage :: String
                      -> String
                      -> HashMap String (HashMap String Integer)
                      -> Double
probWordGivenLanguage word language wordFrequencyByLanguage =
    countWordInLanguage / countWordsUsedInLanguage
  where countWordInLanguage = fromIntegral .
                              lookupDefault 0 word $
                              wordFrequencyByLanguage ! language
        countWordsUsedInLanguage = fromIntegral . sum . elems $ 
                                   wordFrequencyByLanguage ! language

probLanguageGivenMessage :: String
                          -> String
                          -> HashMap String Integer
                          -> HashMap String (HashMap String Integer)
                          -> Double
probLanguageGivenMessage 
    language
    message
    languageFrequency
    wordFrequencyByLanguage = 
        probLanguage language languageFrequency *
        product (L.map (\word -> 
                 probWordGivenLanguage word language
                 wordFrequencyByLanguage)
                (words message))

languageClassifierGivenMessage :: String
                                -> (HashMap String Integer)
                                -> (HashMap String (HashMap String Integer))
                                -> [(String, Double)]
languageClassifierGivenMessage
    message languageFrequency wordFrequencyByLanguage =
        L.map (\language->
              (language,
              probLanguageGivenMessage
                    language message languageFrequency
                    wordFrequencyByLanguage))
              (keys languageFrequency)

maxClassifier :: [(String, Double)] -> (String, Double)
maxClassifier = L.maximumBy (comparing snd)
