{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (runBot) where

import           Control.Lens       hiding ((.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy  as HML (lookup)
import           Data.Monoid
import           Data.Text
import           GHC.Generics
import           Network.URL
import           Network.WebSockets
import           Network.Wreq       as Wreq
import           Text.Printf
import           Wuss

data StartResponse =
  StartResponse {ok  :: Bool
                ,url :: String}
  deriving (Eq,Show,Generic,FromJSON)

data Presence =
  Active
  deriving (Eq,Show,Generic,FromJSON)

data SlackMessage
  = Hello
  | Reconnect {reconnectUrl :: Text}
  | Unknown Object
  | Message {body    :: Text
            ,channel :: Text}
  | PresenceChange {user     :: Text
                   ,presence :: Presence}
  deriving (Eq,Show,Generic)

instance FromJSON SlackMessage where
  parseJSON (Object o) =
    case HML.lookup "type" o of
      Just (String "hello") -> return Hello
      Just (String "message") ->
        do body <- o .: "text"
           channel <- o .: "channel"
           return Message {..}
      Just (String "reconnect_url") ->
        do reconnectUrl <- o .: "url"
           return Reconnect {..}
      Just (String "presence_change") ->
        do user <- o .: "user"
           presence <- o .: "presence"
           return PresenceChange {..}
      _ -> return (Unknown o)
  parseJSON _ = fail "Unrecognised message."

startEndpoint :: String
startEndpoint = "https://slack.com/api/rtm.start"

startSession :: Text -> IO StartResponse
startSession apiKey =
  do r <-
       getWith (defaults & param "token" .~ [apiKey]) startEndpoint >>= asJSON
     return $ r ^. responseBody

runBot :: Text -> IO ()
runBot apiKey =
  do session <- startSession apiKey
     let Just wsurl = importURL $ url session
     let Absolute urlHost = url_type wsurl
     let hostname = host urlHost
     let path = url_path wsurl
     printf "Connecting to: %s %s\n" hostname path
     runSecureClient hostname
                     443
                     ("/" ++ path)
                     botApp

botApp :: Connection -> IO ()
botApp connection =
  do putStrLn "====\nConnected\n===="
     forever $
       do Text dataMessage <- receiveDataMessage connection
          let msg :: Maybe SlackMessage = decode dataMessage
          print msg
          case msg of
            Just (Message t ch) ->
              sendTextData connection $
              encode $
              object ["id" .= (1 :: Int)
                     ,"text" .= ("I heard you say: " <> t)
                     ,"channel" .= ch
                     ,"type" .= pack "message"]
            _ -> return ()
