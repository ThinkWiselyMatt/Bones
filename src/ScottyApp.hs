{-# LANGUAGE OverloadedStrings #-}

module ScottyApp (runScottyApp) where -- scottyApp namespace collision so added run prefix

import Web.Scotty
import CppFFI
import Pyfi (python, py)
import Foreign.C.String (peekCString)
import System.Process (readProcess)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

runScottyApp :: IO ()
runScottyApp = scotty 3001 $ do
    get "/scotty" $ do
        text "Hello from Scotty!"

    get "/scotty/csharp" $ do
        let exePath = "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
        result <- liftIO $ tryReadProcess exePath [] ""
        case result of
            Left err -> text (pack $ "Scotty: " ++ err)
            Right output -> text (pack $ "Scotty: " ++ output)

    get "/scotty/cpp" $ do
      msg <- liftIO (getMessagee >>= peekCString)
      text $ pack $ "Scotty: " ++ msg
      
    get "/scotty/cpp/add/:x/:y" $ do
      x <- param "x"
      y <- param "y"
      sum <- liftIO $ add (read x) (read y)
      text $ pack $ "Scotty: Sum = " ++ show sum
    
    get "/scotty/python/add/:x/:y" $ do
        x <- param "x"
        y <- param "y"
        result <- liftIO $ python "python" $ py "addTwoNumbers" x y
        text $ "Scotty: Sum from Python = " <> (pack . show $ (result :: Int))

    get "/scotty/python/print/:message" $ do
        message <- param "message"
        liftIO $ python "python" $ py "printMessage" message
        text $ "Scotty: Message printed by Python : "


tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))