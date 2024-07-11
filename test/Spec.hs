{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import qualified WebServiceSpecs (spec)
import qualified QuickCheckSpecs (spec)
import TestHelper (startServices, stopServices)
import System.Process (ProcessHandle)
import Control.Concurrent (threadDelay, ThreadId)
import System.IO (Handle)

logOutput :: Bool
logOutput = False -- Set this to True if you want to see logs in console -- logs in text file not working still 

main :: IO ()
main = do
-- Start the services before running the tests
  putStrLn "Starting services..."
  (ph, threads, logFile) <- startServices logOutput
  putStrLn "Services started. Running tests..."
  hspec $ do
    describe "WebService Specs" WebServiceSpecs.spec
    describe "QuickCheck Specs" QuickCheckSpecs.spec
  -- Stop the services after running the tests
  putStrLn "Tests completed. Stopping services..."
  stopServices logOutput (ph, threads, logFile)
  putStrLn "Services stopped."