{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantApp (servantApp) where

import Servant
import Network.Wai.Handler.Warp (run)
import System.Process (readProcess)
import Data.Text.Lazy (Text, fromStrict)
import Data.Text (pack)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

type API = "servant" :> Get '[PlainText] Text
      :<|> "servant" :> "csharp" :> Get '[PlainText] Text

server :: Server API
server = servantHandler :<|> servantCSharpHandler

servantHandler :: Handler Text
servantHandler = return "Hello from Servant!"

servantCSharpHandler :: Handler Text
servantCSharpHandler = do
  let exePath = "Server Dependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
  result <- liftIO $ tryReadProcess exePath [] ""
  case result of
    Left err -> return (fromStrict $ pack $ "Servant: " ++ err)
    Right output -> return (fromStrict $ pack $ "Servant: " ++ output)
    
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

servantApp :: IO ()
servantApp = run 3003 app

tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))
