{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Service.Registration where

import Codec.Serialise
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString as B
import Data.ByteString.Base16 as B16
import Data.ByteString.Builder
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Short as SH
import Data.List as L
import Data.Map.Strict as M
import Data.Serialize
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import Data.Text.Encoding as DTE
import Service.AllpayTransaction
import Service.Data.Allegory
import qualified Service.Data.Allegory as Al
import Service.Env
import Service.Nexa
import Service.ProxyProviderUtxo
import Service.Types
import UtxoPool

import Network.HTTP.Client
import Network.Xoken.Address
import Network.Xoken.Block.Merkle
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Transaction.Common
import qualified Network.Xoken.Transaction.Common as TC (TxOut(..))
import Network.Xoken.Util

import Control.Concurrent.STM
import Control.Exception

import Control.Exception.Lifted as LE
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.List
import Data.Maybe
import Network.Xoken.Constants
import Network.Xoken.Network.Message
import Network.Xoken.Script
import qualified NodeConfig as NC

import Data.Int
import LevelDB
import Service.Merkle

data RegDetails =
    RegDetails
        { addrCom :: String
        , utxoCom :: String
        , exp' :: Int64
        }
    deriving (Show, Eq)

-- create: addressCommitment, utxoCommitment, list of pputxo outpoints and address hashes
getRegistrationDetails ::
       (HasService env m, MonadIO m) => Network -> C.ByteString -> Int -> m (RegDetails, [String], [TxHash], XPubKey)
getRegistrationDetails net pubKey count = do
    let res = A.String (DTE.decodeUtf8 pubKey)
    case parse Prelude.id . xPubFromJSON net $ res of
        Success k -> do
            ops <- getFromPool count
            let addrHashes = getAddressList k count
                utxoRoot = buildMerkleRoot $ outpointHashes ops
                addrRoot = buildMerkleRoot $ addrHashes
                expiry = 2556143999 -- for demo, midnight 31st December 2050
            return $
                ( RegDetails
                      (C.unpack . SH.fromShort $ getHash256 addrRoot)
                      (C.unpack . SH.fromShort $ getHash256 utxoRoot)
                      expiry
                , ops
                , addrHashes
                , k)
        Error err -> throw $ XPubKeyDecodeException (show err)

registerNewUser :: (HasService env m, MonadIO m) => [Int] -> C.ByteString -> Int -> m (C.ByteString, Int64, String)
registerNewUser allegoryName pubKey count = do
    nodeCnf <- getNodeConfig
    let net = NC.bitcoinNetwork nodeCnf
    aMapTvar <- getXPubHashMap
    addressTVar <- getAddressMap
    userValid <- liftIO $ validateUser (NC.nexaHost nodeCnf) (NC.nexaSessionKey nodeCnf) allegoryName
    let ownerUri = fromMaybe (throw NameValidationException) userValid
        feeSats = 100000
    (reg, committedOps, addrHashes, k) <- getRegistrationDetails net pubKey count
    (opRetScript, opRetHash) <- makeOpReturn allegoryName ownerUri reg
    xPubInfo <- liftIO $ readTVarIO aMapTvar
    let f x =
            case x of
                Just v -> Just v
                Nothing -> Just (XPubInfo k count 0 committedOps)
    liftIO $ atomically $ writeTVar aMapTvar (M.alter f (show opRetHash) xPubInfo)
    liftIO $ atomically $ modifyTVar addressTVar (M.insert (xPubExport net k) addrHashes)
    names <- liftIO $ M.keys <$> readTVarIO aMapTvar
    liftIO $ putValue "names" (BSL.toStrict $ A.encode $ nub $ (show opRetHash) : names)
    when (isNothing $ M.lookup (show opRetHash) xPubInfo) $
        liftIO $
        putValue (DTE.encodeUtf8 $ DT.pack (show opRetHash)) (encodeXPubInfo net $ XPubInfo k count 0 committedOps)
    return (opRetScript, feeSats, NC.paymentAddress nodeCnf)

cancelRegistration :: (HasService env m, MonadIO m) => Hash256 -> m ()
cancelRegistration opReturnHash = do
    nodeCnf <- getNodeConfig
    let net = NC.bitcoinNetwork nodeCnf
    aMapTVar <- getXPubHashMap
    addressTVar <- getAddressMap
    aMap <- liftIO $ readTVarIO aMapTVar
    let (XPubInfo xpk count used committedOps) = fromJust $ M.lookup (show opReturnHash) aMap
    -- delete XPubKey registration
    liftIO $ atomically $ modifyTVar aMapTVar $ M.delete $ show opReturnHash
    liftIO $ deleteValue (DTE.encodeUtf8 $ DT.pack $ show opReturnHash)
    -- delete address hashes (Merkle leaves)
    liftIO $ atomically $ modifyTVar addressTVar $ M.delete $ xPubExport net xpk
    -- free up committed utxos; return them to pool
    putBackInPool committedOps
    return ()

makeOpReturn :: (HasService env m, MonadIO m) => [Int] -> String -> RegDetails -> m (C.ByteString, Hash256)
makeOpReturn allegoryName ownerUri reg = do
    providerUri <- NC.proxyProviderUri <$> getNodeConfig
    let al =
            Allegory
                1
                allegoryName
                (OwnerAction
                     (Al.Index 0)
                     (OwnerOutput (Al.Index 1) (Just $ Endpoint "XokenP2P" ownerUri))
                     [ ProxyProvider
                           "AllPay"
                           "Public"
                           (Endpoint "XokenP2P" providerUri)
                           (Registration (addrCom reg) (utxoCom reg) "" (fromIntegral $ exp' reg))
                     ])
        opRetScript = frameOpReturn $ LC.toStrict $ serialise al
        opRetHash = sha256 $ DTE.encodeUtf8 $ encodeHex opRetScript
    return (opRetScript, opRetHash)

validateUser :: (MonadUnliftIO m) => String -> SessionKey -> [Int] -> m (Maybe String)
validateUser host sk name = do
    response <- liftIO $ nexaReq ResellerUri (A.encode $ NexaNameRequest name False) host (Just sk)
    case A.decode (responseBody response) :: Maybe ResellerUriResponse of
        Nothing -> throw NexaResponseException
        Just (ResellerUriResponse gotName uri _ confirmed producer) -> do
            if (gotName /= name) || producer -- if the name doesn't exist or is a producer node
                then return Nothing
                else return $ Just uri

inspectAndRelayRegistrationTx :: (HasService env m, MonadIO m) => C.ByteString -> m Bool
inspectAndRelayRegistrationTx rawTx = do
    nodeCfg <- getNodeConfig
    regMapTVar <- getXPubHashMap
    regMap <- liftIO $ readTVarIO regMapTVar
    case runGetState getConfirmedTx rawTx 0 of
        Left e -> throw RawTxParseException
        Right (mbTx, _) -> do
            let tx@(Tx version ins outs locktime) = fromMaybe (throw RawTxParseException) mbTx
                opRet = TC.scriptOutput $ outs !! 0
                opRetHash = sha256 $ DTE.encodeUtf8 $ encodeHex opRet
                reg = fromMaybe (throw InvalidNameException) $ M.lookup (show opRetHash) regMap
            if verifyPayment (NC.bitcoinNetwork nodeCfg) (NC.paymentAddress nodeCfg) outs
                then do
                    res <- liftIO $ relayTx (NC.nexaHost nodeCfg) (NC.nexaSessionKey nodeCfg) rawTx
                    return $ txBroadcast res
                else do
                    cancelRegistration opRetHash
                    return False

fetchNameFromAllegoryData :: C.ByteString -> [Int]
fetchNameFromAllegoryData allegoryData =
    case decodeOutputScript allegoryData of
        Left err -> throw TxFormatException
        Right os ->
            let allegoryHeader = scriptOps os !! 2
                allegoryData = scriptOps os !! 3
             in case (allegoryHeader, allegoryData) of
                    (OP_PUSHDATA "Allegory/AllPay" OPCODE, OP_PUSHDATA allegory _) ->
                        let alg' = deserialiseOrFail $ LC.fromStrict allegory :: Either DeserialiseFailure Allegory
                         in case alg' of
                                Left df@(DeserialiseFailure b s) -> throw AllegoryMetadataException
                                Right alg -> Al.name alg

verifyPayment :: Network -> String -> [TxOut] -> Bool
verifyPayment net paymentAddrString outs = do
    let paymentScript =
            addressToScriptBS $ fromMaybe (throw PaymentAddressException) $ stringToAddr net (DT.pack paymentAddrString)
        paymentOutputs = L.filter (\(TxOut v s) -> s == paymentScript) outs
        paymentAmount = L.foldr (\(TxOut v0 _) v1 -> (v0 + v1)) 0 paymentOutputs
     in paymentAmount >= 100000

frameOpReturn :: C.ByteString -> C.ByteString
frameOpReturn opReturn = do
    let prefix = (fst . B16.decode) "006a0f416c6c65676f72792f416c6c506179"
    let len = B.length opReturn
    let xx =
            if (len <= 0x4b)
                then word8 $ fromIntegral len
                else if (len <= 0xff)
                         then mappend (word8 0x4c) (word8 $ fromIntegral len)
                         else if (len <= 0xffff)
                                  then mappend (word8 0x4d) (word16LE $ fromIntegral len)
                                  else if (len <= 0x7fffffff)
                                           then mappend (word8 0x4e) (word32LE $ fromIntegral len)
                                           else word8 0x99 -- error scenario!!
    let bs = LC.toStrict $ toLazyByteString xx
    C.append (C.append prefix bs) opReturn
