{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Logger (LoggingT, runLoggingT, logInfoN, runStdoutLoggingT, fromLogStr)
import Control.Monad.IO.Class (liftIO)
import System.IO (openFile, hClose, IOMode(AppendMode), Handle)
import qualified Data.Text.IO as TIO
import qualified ScottyApp
import qualified YesodApp
import qualified ServantApp
import Lib (someFunc, logMessage)
import Data.Text.Encoding (decodeUtf8)
import Control.Logger.Simple

main :: IO ()
main = do
        -- Log the message from someFunc
        liftIO someFunc
        logMain "Starting all applications..."
        void <- forkIO ScottyApp.runScottyApp
        void <- forkIO YesodApp.yesodApp
        void <- forkIO ServantApp.servantApp
        logMain "Applications are running. Press Ctrl+C to exit."

        -- Keep the main thread running indefinitely
        let loop = do
                threadDelay 1000000  -- 1 second
                loop
        liftIO loop

-- Helper function to log messages to a specific main log file
logMain :: String -> IO ()
logMain = logMessage "mainlogfile.txt"