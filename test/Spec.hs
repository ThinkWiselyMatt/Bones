{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import qualified WebServiceSpecs (spec)
import qualified QuickCheckSpecs (spec)
import TestHelper (startServices, stopServices)
import System.Process (ProcessHandle)
import Control.Concurrent (threadDelay, ThreadId)
import System.IO (Handle)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, logInfoN)
import Control.Monad.IO.Class (liftIO)

logOutput :: Bool
logOutput = False -- Set this to True if you want to see logs in console

main :: IO ()
main =  runStdoutLoggingT $ do
    logInfoN "Starting services..."
    (ph, threads, logFile) <- startServices logOutput
    logInfoN "Services started. Running tests..."

    -- Running the tests
    liftIO $ hspec $ do
        describe "WebService Specs" WebServiceSpecs.spec
        describe "QuickCheck Specs" QuickCheckSpecs.spec

    logInfoN "Stopping services..."
    stopServices logOutput (ph, threads, logFile)
    logInfoN "Services stopped."