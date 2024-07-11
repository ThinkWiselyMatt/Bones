{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} --unused libs pulled in by Yesod, no need to warn each build 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module YesodApp (yesodApp) where

import Yesod
import System.Directory (doesFileExist)
import CppFFI
import Foreign.C.String (peekCString)
import System.Environment (setEnv)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text (Text, pack)
import Lib (tryReadProcess, tryCallCommand, logMessage)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status200, status400, status404, status500)


data App = App

mkYesod "App" [parseRoutes|
/yesod YesodR GET
/yesod/csharp YesodCSharpR GET
/yesod/cpp YesodCppGetMessageR GET
/yesod/cpp/add/#Int/#Int YesodCppAddR GET
/yesod/python/#String PythonR GET
|]

instance Yesod App

getYesodR :: HandlerFor App Html
getYesodR = defaultLayout [whamlet|Hello from Yesod!|]

getYesodCSharpR :: HandlerFor App T.Text
getYesodCSharpR = do
    result <- liftIO $ tryReadProcess "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe" [] ""
    case result of
        Left err -> return (T.pack $ "Yesod: " ++ err)
        Right output -> return (T.pack $ "Yesod: " ++ output)

getYesodCppGetMessageR :: HandlerFor App T.Text
getYesodCppGetMessageR = do
    message <- liftIO $ getMessagee >>= peekCString
    return $ T.pack $ "Yesod: " ++ message

getYesodCppAddR :: Int -> Int -> HandlerFor App T.Text
getYesodCppAddR x y = do
    sumResult <- liftIO $ add (fromIntegral x) (fromIntegral y)
    return $ T.pack $ "Yesod: Sum = " ++ show sumResult

getPythonR :: String -> Handler Html
getPythonR filename = do
    let filepath = "ServerDependancies\\PythonScripts\\" ++ filename
    fileExists <- liftIO $ doesFileExist filepath
    if not fileExists
    then sendResponseStatus status404 (LT.pack "File does not exist")
    else if not (".py" `List.isSuffixOf` filename)
        then sendResponseStatus status400 (LT.pack "File is not a Python (.py) file")
        else do
            result <- liftIO $ tryCallCommand filepath
            case result of
                Left err -> sendResponseStatus status500 (LT.pack $ "Error: " ++ err)
                Right () -> sendResponseStatus status200 (T.pack "Python script executed successfully")
    
yesodApp :: IO ()
yesodApp = do
    logYesod "Setting environment variable for DLL path..."
    liftIO $ setEnv "PATH" "ServerDependancies\\C++NativeExports"
    logYesod "Starting Yesod App..."
    liftIO $ warp 3002 App

-- Helper function to log messages to a specific Yesod log file
logYesod :: String -> IO ()
logYesod = logMessage "yesodlogfile.txt"