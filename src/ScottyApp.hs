{-# LANGUAGE OverloadedStrings #-}

module ScottyApp (runScottyApp) where -- scottyApp namespace collision so added run prefix

import Web.Scotty
import System.Process (readProcess)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (catch, SomeException)

runScottyApp :: IO ()
runScottyApp = scotty 3001 $ do
    get "/scotty" $ do
        text "Hello from Scotty!"

    get "/scotty/csharp" $ do
        let exePath = "C:\\Users\\Owner\\Desktop\\Development\\Bones\\Server Dependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
        result <- liftIO $ tryReadProcess exePath [] ""
        case result of
            Left err -> text (pack $ "Scotty: " ++ err)
            Right output -> text (pack $ "Scotty: " ++ output)


tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))