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
logOutput = True -- Set this to True if you want to see logs in console -- logs in text file not working still 

main :: IO ()
main = do
  -- Start the services before running the tests
  (ph, threads, logFile) <- startServices logOutput
  hspec $ do
    describe "WebService Specs" WebServiceSpecs.spec
    describe "QuickCheck Specs" QuickCheckSpecs.spec
  -- Stop the services after running the tests
  stopServices logOutput (ph, threads, logFile)