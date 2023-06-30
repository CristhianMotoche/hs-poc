{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Neo.Slack (Status(..), StatusResponse(..), updateStatus) where

import qualified Data.Aeson              as A
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as LB
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS

import           Neo.Config              (Config (..))



data StatusResponse = StatusResponse
    { ok     :: Bool
    , status :: Maybe Status
    }

instance A.FromJSON StatusResponse where
  parseJSON (A.Object v) =
    StatusResponse <$> v A..: "ok" <*> pure Nothing
  parseJSON _ = error "Failed at parsing Status"

data Status = Status
    { text       :: String
    , emoji      :: String
    , expiration :: Int
    }


instance A.ToJSON Status where
  toJSON (Status {..}) = A.object [
    "profile" A..= A.object [
        "status_text" A..= text
        , "status_emoji" A..= emoji
        , "status_expiration" A..= expiration
        ]
    ]


endpoint :: Config -> Status -> Request
endpoint (Config token) status = "https://api.slack.com/api/users.profile.set"
  { method = "POST"
  , secure = True
  , requestBody = RequestBodyLBS $ A.encode status
  , requestHeaders = [
      ("Content-type", "application/json; charset=utf-8"),
      ("Authorization", "Bearer " <> B.pack token)
    ]
  }


updateStatus :: Config -> Status -> IO (Maybe StatusResponse)
updateStatus config status = do
  man <- TLS.newTlsManager
  resp <- httpLbs (endpoint config status) man
  return $ A.decode $ responseBody resp
