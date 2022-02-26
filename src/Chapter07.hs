{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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

myoauth :: OAuth
myoauth = newOAuth {
    oauthServerName = "api.twitter.com",
    oauthConsumerKey = "CONSUMER KEY",
    oauthConsumerSecret = "CONSUMER SECRET KEY"
}

mycred :: Credential
mycred = newCredential "ACCESS TOKEN" "ACESS TOKEN SECRET"

data User = User {
    screenName :: !String
} deriving (Show, Generic)

data Status = Status {
    text :: !String,
    lang :: !String,
    user :: !User
} deriving (Show, Generic)

data Search = Search {
    statuses :: ![Status]
} deriving (Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJSON Status
instance ToJSON Status
instance FromJSON Search
instance ToJSON Search

twitterSearch :: String -> IO (Either String Search)
twitterSearch term = do 
    req <- parseUrl $ 
            "https://api.twitter.com/1.1/search/tweets.json?count=100&q=" ++
            term 
    res <- withManager $ \m -> do 
                signedreq <- singOAuth myoauth mycred req
                httpLbs signedreq m 
    return $ eitherDecode $ responseBody res