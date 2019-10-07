 
{-# LANGUAGE OverloadedStrings #-}

import qualified MultiplicationService
import MultiplicationService_Iface
import SharedService_Iface
import Shared_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Server

import Data.Int
import Data.String
import Data.Maybe
import Text.Printf
import Control.Exception (throw)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Map ((!))
import Data.Monoid

data MultiplicationServiceHandler = MultiplicationServiceHandler {mathLog :: MVar (M.Map Int32 SharedStruct)}

newMultiplicationServiceHandler = do
  log <- newMVar mempty
  return $ MultiplicationServiceHandler log

instance SharedService_Iface MultiplicationServiceHandler where
  getStruct self k = do
    myLog <- readMVar (mathLog self)
    return $ (myLog ! k)


instance MultiplicationService_Iface MultiplicationServiceHandler where

  multiply n1 n2 = do
    return (n1 * n2)




main =  do
  handler <- newMultiplicationServiceHandler
  print "Starting the server..."
  runBasicServer handler MultiplicationService.process 9090
