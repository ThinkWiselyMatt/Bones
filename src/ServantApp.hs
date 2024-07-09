{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantApp (servantApp) where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import CppFFI
import Foreign.C.String (peekCString)
import System.Environment (setEnv)
import Lib (tryReadProcess)

type API = "servant" :> Get '[PlainText] Text
      :<|> "servant" :> "csharp" :> Get '[PlainText] Text
      :<|> "servant" :> "cppgetmessagee" :> Get '[PlainText] Text
      :<|> "servant" :> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] Text

server :: Server API
server = servantHandler :<|> servantCSharpHandler :<|> servantCppGetMessageHandler :<|> servantCppAddHandler

servantHandler :: Handler Text
servantHandler = return "Hello from Servant!"

servantCSharpHandler :: Handler Text
servantCSharpHandler = do
  let exePath = "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
  result <- liftIO $ tryReadProcess exePath [] ""
  case result of
    Left err -> return $ LT.pack $ "Servant: " ++ err
    Right output -> return $ LT.pack $ "Servant: " ++ output

servantCppGetMessageHandler :: Handler Text
servantCppGetMessageHandler = liftIO $ do
  message <- getMessagee >>= peekCString
  return $ LT.pack $ "Servant: " ++ message
    
servantCppAddHandler :: Int -> Int -> Handler Text
servantCppAddHandler x y = do
  result <- liftIO $ add (fromIntegral x) (fromIntegral y)
  return $ LT.pack $ "Servant: Sum = " ++ show result

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

servantApp :: IO ()
servantApp = do
  -- Set the DLL path
  setEnv "PATH" "ServerDependencies\\C++NativeExports"
  run 3003 app

