{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantApp (servantApp) where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)

type API = "servant" :> Get '[PlainText] Text

server :: Server API
server = return "Hello from Servant!"

api :: Proxy API
api = Proxy

servantApp :: IO ()
servantApp = run 3003 (serve api server)
