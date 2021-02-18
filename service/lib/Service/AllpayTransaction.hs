{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Service.AllpayTransaction where

import Codec.Compression.GZip as GZ
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, mapConcurrently, mapConcurrently_, race_)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Exception.Lifted as LE
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.STM
import Crypto.Secp256k1

import Data.Aeson as A
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import Data.Default
import Data.Hashable
import Data.IORef
import Data.Int
import Data.List
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import qualified Data.Serialize as S
import Data.String (IsString, fromString)
import Data.Text (Text(..))
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as E
import Data.Word (Word32, Word64, Word8)
import LevelDB
import Network.Xoken.Address
import Network.Xoken.Block.Merkle
import Network.Xoken.Constants
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Script
import Network.Xoken.Transaction
import Network.Xoken.Transaction.Common
import NodeConfig
import Service.Env
import qualified Service.Env as SE (ProxyProviderUtxo(..))
import Service.ProxyProviderUtxo
import Service.Subscriber
import Service.Types
import System.Logger as LG

getPartiallySignedAllpayTransaction ::
       (HasService env m, MonadIO m)
    => [(OutPoint', Int64)] -- standard inputs, corresponding input value
    -> Int64 -- value
    -> String -- receiver
    -> String -- change address
    -> [Text] -- OP_RETURN push data
    -> m (BC.ByteString, [(String, Bool)], [(String, Bool)]) -- serialized transaction
getPartiallySignedAllpayTransaction inputs amount recipient changeAddr opReturnData = do
    lg <- getLogger
    nodeCnf <- getNodeConfig
    let net = bitcoinNetwork nodeCnf
    poolAddr <- poolAddress <$> getNodeConfig
    poolSecKey <- poolSecKey <$> getNodeConfig
    let opReturn = makeOpReturn opReturnData
    res <- LE.try $ generateAddress recipient
    let inputsOp =
            (\(op', val) ->
                 (OutPoint (fromJust $ hexToTxHash $ DT.pack $ opTxHash op') (fromIntegral $ opIndex op'), val)) <$>
            inputs
    case res of
        Left (e :: SomeException) -> do
            err lg $
                LG.msg $ BC.pack $ "[ERROR] Failed to generate address for subscriber " <> recipient <> ": " <> show e
            throw $ AddressGenerationException (show e)
        Right (address, pputxo, addrProof, utxoProof) -> do
            let ppOutPoint =
                    OutPoint
                        (fromJust $ hexToTxHash $ DT.pack $ SE.txid $ pputxo)
                        (fromIntegral $ SE.outputIndex $ pputxo)
            let addr = DT.unpack . fromJust $ addrToString net address
            let inputs' = ppOutPoint : ((\(outpoint, _) -> outpoint) <$> inputsOp)
            let fee = guessTxFee (fromIntegral 5) (1 + length inputs) 2
            -- compute change
            let totalInput = L.foldl (+) 0 $ (\(_, val) -> val) <$> inputsOp
            let values = (SE.value pputxo) : ((\(_, value) -> fromIntegral value) <$> inputsOp)
            let change = totalInput - (amount + fromIntegral fee)
            -- add proxy-provider utxo output
            let outputs =
                    [ (DT.pack poolAddr, fromIntegral $ SE.value $ pputxo)
                    , (DT.pack addr, fromIntegral amount)
                    , (DT.pack changeAddr, fromIntegral change)
                    ]
            case buildAddrTx net inputs' outputs of
                Left e -> do
                    err lg $ LG.msg $ BC.pack $ "[ERROR] Failed to build AllPay transaction: " <> e
                    throw $ TransactionBuildingException e
                Right (Tx version ins outs locktime) -> do
                    case decodeOutputBS (BC.pack $ SE.scriptPubKey pputxo) of
                        Left e -> do
                            err lg $ LG.msg $ BC.pack $ "[ERROR] Failed to decode proxy-provider UTXO script: " <> e
                            throw ProxyUtxoException
                        Right so -> do
                            let si =
                                    SigInput
                                        so
                                        (fromIntegral $ SE.value pputxo)
                                        ppOutPoint
                                        (setForkIdFlag sigHashAll)
                                        Nothing
                            case signTx net (Tx version ins (TxOut 0 opReturn : outs) locktime) [si] [poolSecKey] of
                                Left e -> do
                                    err lg $ LG.msg $ BC.pack $ "[ERROR] Failed to sign AllPay transaction: " <> e
                                    throw $ TransactionSigningException e
                                Right psaTx -> do
                                    let serializedTx = BSL.toStrict $ A.encode $ createTx' psaTx values
                                    return $
                                        ( serializedTx
                                        , (\(h, l) -> (BC.unpack h, l)) <$> addrProof
                                        , (\(h, l) -> (BC.unpack h, l)) <$> utxoProof)

makeOpReturn :: [Text] -> ByteString
makeOpReturn opReturnData =
    let decodedData = fromMaybe (throw InvalidOpReturnDataException) <$> decodeHex <$> opReturnData
     in L.foldr B.append mempty $ S.encode <$> OP_0 : OP_RETURN : (opPushData <$> decodedData)

createTx' :: Tx -> [Int] -> Tx'
createTx' (Tx version inputs outs locktime) values =
    Tx' {txVersion = version, txIn = fmap func $ Prelude.zip inputs values, txOut = outs, txLockTime = locktime}
  where
    func (TxIn prevOut scriptIn txInSeq, val) =
        TxIn' {prevOutput = prevOut, scriptInput = scriptIn, txInSequence = txInSeq, value = fromIntegral val}
