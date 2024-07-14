{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( 
        someFunc,
        tryReadProcess,
        tryCallCommand,
        logMessage
    ) where

import qualified Data.Text.IO as T
import System.Process (readProcess, callCommand)
import Control.Exception (catch, SomeException)
import Control.Logger.Simple
import Data.Text (Text, pack)
import System.FilePath ((</>))
import System.Info (os)

someFunc :: IO ()
someFunc = T.putStrLn "From bones to a skeleton"

tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = 
    catch (Right <$> readProcess cmd args input)
          (return . Left . show :: SomeException -> IO (Either String String))

tryCallCommand :: FilePath -> IO (Either String ())
tryCallCommand cmd = 
    catch (callCommand cmd >> return (Right ())) 
          (return . Left . show :: SomeException -> IO (Either String ()))

logMessage :: FilePath -> String -> IO ()
logMessage logFile msg = 
    let fullPath = "logs" </> logFile
    in withGlobalLogging (LogConfig (Just logFile) True) $ logInfo (pack msg)