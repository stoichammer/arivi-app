{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import Data.Aeson as A
import Data.String
import GHC.Generics
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS

type SessionKey = String

data AuthRequestResponse =
    AuthRequestResponse
        { auth :: Auth
        }
    deriving (Show, Eq, Generic)

instance FromJSON AuthRequestResponse

data Auth =
    Auth
        { callsRemaining :: Int
        , callsUsed :: Int
        , sessionKey :: Maybe String
        }
    deriving (Show, Eq, Generic)

instance FromJSON Auth

getNexaSessionKey :: String -> String -> String -> IO (Either String SessionKey)
getNexaSessionKey nexaAddr user pass = do
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
    initRequest <- parseRequest $ "https://" <> nexaAddr <> "/v1/auth"
    let request =
            initRequest
                { method = "POST"
                , requestHeaders = [("content-type", "application/json")]
                , requestBody = RequestBodyLBS $ A.encode $ object ["username" .= user, "password" .= pass]
                }
    response <- httpLbs request manager
    case A.decode (responseBody response) :: Maybe AuthRequestResponse of
        Nothing -> return $ Left "bad request: check host config"
        Just (AuthRequestResponse a) -> do
            case sessionKey a of
                Nothing -> return $ Left "incorrect username/password"
                Just s -> return $ Right s
