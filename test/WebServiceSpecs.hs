{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module WebServiceSpecs (spec) where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import System.IO (hGetContents, hPutStr, hPutStrLn, stderr, openFile, IOMode(WriteMode), hFlush)
import System.Process (createProcess, proc, terminateProcess, waitForProcess, ProcessHandle, StdStream(CreatePipe), std_out, std_err)
import Data.Text (isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (when, void)

spec :: Spec
spec = beforeAll startServices $ afterAll stopServices $ do
  describe "Web Service Apps" $ do
    it "responds from Scotty app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3001/scotty"
      response <- httpLbs request manager
      L8.unpack (responseBody response) `shouldBe` "Hello from Scotty!"

    it "responds from Yesod app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3002/yesod"
      response <- httpLbs request manager
      let body = decodeUtf8 (L8.toStrict $ responseBody response)
      body `shouldSatisfy` ("Hello from Yesod!" `isInfixOf`)

    it "responds from Servant app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3003/servant"
      response <- httpLbs request manager
      L8.unpack (responseBody response) `shouldBe` "Hello from Servant!"

startServices :: IO (ProcessHandle, [ThreadId])
startServices = do
  (_, Just hout, Just herr, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
    { std_out = CreatePipe, std_err = CreatePipe }
  -- Capture and log the output if needed
  let logOutput = False -- Set this to True if you want to see logs
  logFile <- openFile "test_output.log" WriteMode
  let handleOutput handle = forkIO $ do
        contents <- hGetContents handle
        if logOutput
          then mapM_ (hPutStrLn stderr) (lines contents)
          else hPutStr logFile contents >> hFlush logFile
  outThread <- handleOutput hout
  errThread <- handleOutput herr
  -- Give the services some time to start
  threadDelay 5000000  -- 5 seconds
  return (ph, [outThread, errThread])

stopServices :: (ProcessHandle, [ThreadId]) -> IO ()
stopServices (ph, threads) = do
  mapM_ killThread threads
  terminateProcess ph
  _ <- waitForProcess ph
  return ()
