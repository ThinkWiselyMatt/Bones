{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import qualified WebServiceSpecs (spec)
import qualified QuickCheckSpecs (spec)
import TestHelper (startServices, stopServices)
import System.Process (ProcessHandle)
import Control.Concurrent (threadDelay, ThreadId)
import System.IO (Handle)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)

logOutput :: Bool
logOutput = False -- Set this to True if you want to see logs in console -- logs in text file not working still 

main :: IO ()
main = do
    putStrLn "Starting services..."
    (ph, threads, logFile) <- startServices logOutput
    putStrLn "Services started. Running tests..."

    -- Running the tests
    hspec $ do
        describe "WebService Specs" WebServiceSpecs.spec
        describe "QuickCheck Specs" QuickCheckSpecs.spec

    putStrLn "Stopping services..."
    runStdoutLoggingT $ stopServices logOutput (ph, threads, logFile)
    putStrLn "Services stopped."