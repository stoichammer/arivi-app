{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP.Handler where

import Control.Applicative ((<|>))
import qualified Control.Error.Util as Extra
import Control.Exception (SomeException(..), throw, try)
import qualified Control.Exception.Lifted as LE (try)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.State.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Encoding as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Either as Either
import qualified Data.HashTable.IO as H
import Data.Int
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Serialize as S
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Service.Env
import HTTP.Types
import Snap
import Service.AllpayTransaction
import Service.Registration
import qualified System.Logger as LG

registerNewUser' :: ReqParams' -> Handler App App ()
registerNewUser' (Register rname xpk nutxo retaddr count) = do
    res <- registerNewUser rname xpk count nutxo retaddr
    case res of
        Left e -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right ops -> do
            writeBS $ BSL.toStrict $ encodeResp True $ (Just $ RespRegister ops)
registerNewUser' _ = throwBadRequest

getPartiallySignedAllpayTransaction' :: ReqParams' -> Handler App App ()
getPartiallySignedAllpayTransaction' (PSAllpayTransaction inputs recipient amount change) = do
    res <- getPartiallySignedAllpayTransaction inputs amount recipient change
    case res of
        Left e -> do
            modifyResponse $ setResponseStatus 500 "Internal Server Error"
            writeBS "INTERNAL_SERVER_ERROR"
        Right (stx, addrProof, utxoProof) -> do
            writeBS $ BSL.toStrict $ encodeResp True $ (Just $ RespPSAllpayTransaction stx addrProof utxoProof)
getPartiallySignedAllpayTransaction' _ = throwBadRequest

throwBadRequest :: Handler App App ()
throwBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "Bad Request"

encodeResp :: Aeson.ToJSON a => Bool -> a -> C.ByteString
encodeResp True = AP.encodePretty
encodeResp False = Aeson.encode

withReq :: Aeson.FromJSON a => (a -> Handler App App ()) -> Handler App App ()
withReq handler = do
    rq <- getRequest
    let ct = getHeader "content-type" rq <|> (getHeader "Content-Type" rq) <|> (getHeader "Content-type" rq)
    if ct == Just "application/json"
        then do
            bsReq <- readRequestBody (8 * 2048)
            case Aeson.eitherDecode bsReq of
                Right r -> handler r
                Left err -> do
                    modifyResponse $ setResponseStatus 400 "Bad Request"
                    writeBS "400 error"
        else throwBadRequest