{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import qualified WebServiceSpecs (spec)
import qualified QuickCheckSpecs (spec)
import TestHelper (startServices, stopServices)
import System.Process (ProcessHandle)
import Control.Concurrent (threadDelay, ThreadId)
import Control.Monad.IO.Class (liftIO)
import Lib(logMessage)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
    createDirectoryIfMissing True "logs"
    logSpec "Starting services..."
    ph <- startServices
    logSpec "Services started. Running tests..."

    -- Running the tests
    liftIO $ hspec $ do
        describe "WebService Specs" WebServiceSpecs.spec
        describe "QuickCheck Specs" QuickCheckSpecs.spec

    logSpec "Stopping services..."
    stopServices ph
    logSpec "Services stopped."

-- Helper function to log messages to a specific log file
logSpec :: String -> IO ()
logSpec = logMessage "logs/spec.txt"