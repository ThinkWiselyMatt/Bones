{-# LANGUAGE OverloadedStrings #-}

module ScottyApp (runScottyApp) where -- scottyApp namespace collision so added run prefix

import Web.Scotty
import System.Directory (doesFileExist)
import CppFFI
import Foreign.C.String (peekCString)
import Data.Text.Lazy (pack, isSuffixOf)
import qualified Data.Text.Lazy as LT
import Control.Monad.IO.Class (liftIO)
import Lib (tryReadProcess, tryCallCommand)

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
      x <- captureParam "x"
      y <- captureParam "y"
      sumResult <- liftIO $ add (read x) (read y)
      text $ "Scotty: Sum = " <> (pack . show $ sumResult)

    get "/scotty/python/:filename" $ do
        filename <- captureParam "filename"
        let filepath = "ServerDependancies\\PythonScripts\\" ++ filename
        fileExists <- liftIO $ doesFileExist filepath
        if not fileExists
            then text "File does not exist"
            else if not (LT.pack ".py" `isSuffixOf` LT.pack filename)
                then text "File is not a Python (.py) file" --maybe this doesnt matter in the future, but for now... 
                else do
                    result <- liftIO $ tryCallCommand ("python " ++ filepath)
                    case result of
                        Left err -> text (pack $ "Error: " ++ err)
                        Right _ -> text "Python script executed successfully"