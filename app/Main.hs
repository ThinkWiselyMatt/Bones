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
import Lib (someFunc)
import Data.Text.Encoding (decodeUtf8)

main :: IO ()
main = do
    -- Open the log file
    logFile <- openFile "service_output.log" AppendMode
    -- Start logging within the LoggingT monad
    runStdoutLoggingT $ do
        -- Log the message from someFunc
        liftIO someFunc
        logInfoN "Starting all applications..."
        _ <- liftIO $ forkIO $ runLoggingToFile ScottyApp.runScottyApp logFile
        _ <- liftIO $ forkIO $ runLoggingToFile YesodApp.yesodApp logFile
        _ <- liftIO $ forkIO $ runLoggingToFile ServantApp.servantApp logFile
        logInfoN "Applications are running. Press Ctrl+C to exit."

        -- Keep the main thread running indefinitely
        let loop = do
                threadDelay 1000000  -- 1 second
                loop
        liftIO loop
        liftIO $ hClose logFile

runLoggingToFile :: LoggingT IO () -> Handle -> IO ()
runLoggingToFile action logFile = runLoggingT action (\_ _ _ msg -> TIO.hPutStrLn logFile (decodeUtf8 $ fromLogStr msg))
