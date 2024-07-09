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
import Pyfi (python, py)
import Lib (tryReadProcess)

type API = "servant" :> Get '[PlainText] Text
      :<|> "servant" :> "csharp" :> Get '[PlainText] Text
      :<|> "servant" :> "cpp" :> Get '[PlainText] Text
      :<|> "servant" :> "cpp" :> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] Text
      :<|> "servant" :> "python" :> "add" :> Capture "x" Int :> Capture "y" Int :> Get '[PlainText] String
      :<|> "servant" :> "python" :> "print" :> Capture "message" String :> Get '[PlainText] String

server :: Server API
server = servantHandler :<|> servantCSharpHandler :<|> servantCppGetMessageHandler :<|> servantAddHandler :<|> pythonAddHandler :<|> pythonPrintHandler

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

servantPythonAddHandler :: Int -> Int -> Handler String
servantPythonAddHandler x y = do
  result <- liftIO $ python "python" $ py "addTwoNumbers" x y
  return $ "Servant: Sum from Python = " ++ show (result :: Int)

servantPythonPrintHandler :: String -> Handler Text
servantPythonPrintHandler message = do
  liftIO $ python "python" $ py "printMessage" message
  return $ "Servant: Message printed by Python"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

servantApp :: IO ()
servantApp = do
  -- Set the DLL path
  setEnv "PATH" "ServerDependencies\\C++NativeExports"
  run 3003 app

