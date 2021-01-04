{-# LANGUAGE OverloadedStrings #-}

module HTTP.Server where

import qualified Data.ByteString as B
import Service.Env
import HTTP.Handler
import HTTP.Types (App(..))
import Snap

appInit :: AllpayProxyEnv -> SnapletInit App App
appInit env =
    makeSnaplet "v1" "API's" Nothing $ do
        addRoutes apiRoutes
        return $ App env

apiRoutes :: [(B.ByteString, Handler App App ())]
apiRoutes = [("/v1/register", method POST (withReq registerNewUser'))
            ,("/v1/ps-allpay-tx", method POST (withReq getPartiallySignedAllpayTransaction'))
            ]