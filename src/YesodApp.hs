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
import System.Process (readProcess)
import Data.Text (Text, pack)
import Control.Exception (catch, SomeException)

data App = App

mkYesod "App" [parseRoutes|
/yesod YesodR GET
/yesod/csharp YesodCSharpR GET
|]

instance Yesod App

getYesodR :: HandlerFor App Html
getYesodR = defaultLayout [whamlet|Hello from Yesod!|]

getYesodCSharpR :: HandlerFor App Text
getYesodCSharpR = do
  result <- liftIO $ tryReadProcess "Server Dependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe" [] ""
  case result of
    Left err -> return (pack $ "Yesod: " ++ err)
    Right output -> return (pack $ "Yesod: " ++ output)
    
yesodApp :: IO ()
yesodApp = warp 3002 App

tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))