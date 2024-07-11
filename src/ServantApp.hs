{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantApp (servantApp) where

import Servant
import System.Directory (doesFileExist)
import Network.Wai.Handler.Warp (run)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text, pack)
import CppFFI
import Foreign.C.String (peekCString)
import System.Environment (setEnv)
import Lib (tryReadProcess, tryCallCommand)
import Control.Monad.Logger (LoggingT, logInfoN)
import Control.Monad.IO.Class (liftIO)

type API = "servant" :> Get '[PlainText] Text
      :<|> "servant" :> "csharp" :> Get '[PlainText] Text
      :<|> "servant" :> "cpp" :> Get '[PlainText] Text
      :<|> "servant" :> "cpp" :> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] Text
      :<|> "servant" :> "python" :> Capture "filename" String :> Get '[PlainText] Text

server :: Server API
server = servantHandler :<|> servantCSharpHandler :<|> servantCppGetMessageHandler :<|> servantCppAddHandler :<|> servantPythonScriptHandler

servantHandler :: Handler Text
servantHandler = return "Hello from Servant!"

servantCSharpHandler :: Handler Text
servantCSharpHandler = do
  let exePath = "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
  result <- liftIO $ tryReadProcess exePath [] ""
  case result of
    Left err -> return $ T.pack $ "Servant: " ++ err
    Right output -> return $ T.pack $ "Servant: " ++ output

servantCppGetMessageHandler :: Handler Text
servantCppGetMessageHandler = liftIO $ do
  message <- getMessagee >>= peekCString
  return $ T.pack $ "Servant: " ++ message

servantCppAddHandler :: Int -> Int -> Handler Text
servantCppAddHandler x y = do
  result <- liftIO $ add (fromIntegral x) (fromIntegral y)
  return $ T.pack $ "Servant: Sum = " ++ show result

servantPythonScriptHandler :: String -> Handler Text
servantPythonScriptHandler filename = do
    let filepath = "ServerDependancies\\PythonScripts\\" ++ filename
    fileExists <- liftIO $ doesFileExist filepath
    if not fileExists
        then return $ T.pack "File does not exist"
        else if not (".py" `List.isSuffixOf` filename)
            then return $ T.pack "File is not a Python (.py) file"
            else do
                result <- liftIO $ tryCallCommand filepath
                case result of
                    Left err -> return $ T.pack $ "Error: " ++ err
                    Right () -> return $ T.pack "Python script executed successfully"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

servantApp :: LoggingT IO ()
servantApp = do
  logInfoN "Setting environment variable for DLL path..."
  liftIO $ setEnv "PATH" "ServerDependencies\\C++NativeExports"
  logInfoN "Starting Servant App..."
  liftIO $ run 3003 app