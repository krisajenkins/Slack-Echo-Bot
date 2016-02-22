{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (runBot) where

import           Control.Lens
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.URL
import           Network.WebSockets
import           Network.Wreq       as Wreq
import           Text.Printf
import           Wuss

botApp :: Connection -> IO ()
botApp connection =
  do print ("Connected" :: String)
     msg <- receiveDataMessage connection
     print msg
     botApp connection

apiKey :: Text
apiKey = ""

data StartResponse =
  StartResponse {ok  :: Bool
                ,url :: String}
  deriving (Eq,Show,Generic)

instance FromJSON StartResponse

startEndpoint :: String
startEndpoint = "https://slack.com/api/rtm.start"

startSession :: IO StartResponse
startSession =
  do r <-
       getWith (defaults & param "token" .~ [apiKey]) startEndpoint >>= asJSON
     return $r ^. responseBody

runBot :: IO ()
runBot =
  do session <- startSession
     let Just wsurl = importURL $ url session
     let Absolute urlHost = url_type wsurl
     let hostname = host urlHost
     let path = url_path wsurl
     printf "Connecting to: %s %s" hostname path
     runSecureClient hostname
                     443
                     ("/" ++ path)
                     botApp
