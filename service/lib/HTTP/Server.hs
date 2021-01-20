{-# LANGUAGE OverloadedStrings #-}

module HTTP.Server where

import qualified Data.ByteString as B
import HTTP.Handler
import HTTP.Types (App(..))
import Service.Env
import Snap
import Snap.Util.CORS

appInit :: AllpayProxyEnv -> SnapletInit App App
appInit env =
    makeSnaplet "v1" "API's" Nothing $ do
        addRoutes apiRoutes
        wrapSite (applyCORS defaultOptions)
        return $ App env

apiRoutes :: [(B.ByteString, Handler App App ())]
apiRoutes =
    [ ("/v1/register", method POST (withReq registerNewUser'))
    , ("/v1/ps-allpay-tx", method POST (withReq getPartiallySignedAllpayTransaction'))
    , ("/v1/faucet/:address", method GET (getCoins'))
    ]
