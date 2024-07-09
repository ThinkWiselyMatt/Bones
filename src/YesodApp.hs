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
import CppFFI
import Foreign.C.String (peekCString)
import System.Environment (setEnv)
import System.Process (readProcess)
import Data.Text (Text, pack)
import Control.Exception (catch, SomeException)

data App = App

mkYesod "App" [parseRoutes|
/yesod YesodR GET
/yesod/csharp YesodCSharpR GET
/yesod/cppgetmessagee YesodCppGetMessageR GET
/yesod/add/#Int/#Int YesodAddR GET
|]

instance Yesod App

getYesodR :: HandlerFor App Html
getYesodR = defaultLayout [whamlet|Hello from Yesod!|]

getYesodCSharpR :: HandlerFor App Text
getYesodCSharpR = do
  result <- liftIO $ tryReadProcess "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe" [] ""
  case result of
    Left err -> return (pack $ "Yesod: " ++ err)
    Right output -> return (pack $ "Yesod: " ++ output)

getYesodCppGetMessageR :: HandlerFor App Text
getYesodCppGetMessageR = do
  message <- liftIO $ getMessagee >>= peekCString
  return $ pack $ "Yesod: " ++ message

getYesodAddR :: Int -> Int -> HandlerFor App Text
getYesodAddR x y = do
  sum <- liftIO $ add (fromIntegral x) (fromIntegral y)
  return $ pack $ "Yesod: Sum = " ++ show sum
    
yesodApp :: IO ()
yesodApp = do
  -- Set the DLL path
  setEnv "PATH" "ServerDependencies\\C++NativeExports"
  warp 3002 App

tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))