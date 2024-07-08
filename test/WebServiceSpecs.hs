{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module WebServiceSpecs (spec) where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import System.IO (hGetContents, hPutStr, hPutStrLn, stderr, openFile, IOMode(AppendMode), hFlush, Handle, hClose)
import System.Process (createProcess, proc, terminateProcess, waitForProcess, ProcessHandle, StdStream(CreatePipe), std_out, std_err)
import Data.Text (isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (forM_)

spec :: Spec
spec = beforeAll (startServices logOutput) $ afterAll (stopServices logOutput) $ do
  describe "Web Service Apps" $ do
    it "responds from Scotty app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3001/scotty"
      response <- httpLbs request manager
      L8.unpack (responseBody response) `shouldBe` "Hello from Scotty!"

    it "responds from Scotty app CSharp" $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3001/scotty/csharp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Scotty:"
      body `shouldContain` "Hello from C#"

    it "responds from Yesod app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3002/yesod"
      response <- httpLbs request manager
      let body = decodeUtf8 (L8.toStrict $ responseBody response)
      body `shouldSatisfy` ("Hello from Yesod!" `isInfixOf`)

    it "responds from Yesod app CSharp" $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3002/yesod/csharp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Yesod:"
      body `shouldContain` "Hello from C#"

    it "responds from Servant app" $ \_ -> do
      manager <- newManager tlsManagerSettings
      request <- parseRequest "http://localhost:3003/servant"
      response <- httpLbs request manager
      L8.unpack (responseBody response) `shouldBe` "Hello from Servant!"

    it "responds from Servant app CSharp" $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3003/servant/csharp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Servant:"
      body `shouldContain` "Hello from C#"

    it "hit enter to continue" $ \_ -> do
      (1 + 1) `shouldBe` 2

logOutput :: Bool
logOutput = False -- Set this to True if you want to see logs

startServices :: Bool -> IO (ProcessHandle, [ThreadId], Handle)
startServices logOutput = do
  logFile <- openFile "test_output.log" AppendMode -- This will append to the file (doesn't work still empty log file TODO)
  (_, Just hout, Just herr, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
    { std_out = CreatePipe, std_err = CreatePipe }
  
  let handleOutput handle = forkIO $ do
        contents <- hGetContents handle
        putStrLn "Reading output from handle..." -- Debug statement
        if logOutput
          then mapM_ (hPutStrLn stderr) (lines contents)
          else do
            hPutStr logFile contents
            hFlush logFile
            hClose logFile
  
  outThread <- handleOutput hout
  errThread <- handleOutput herr

  -- Give the services some time to start
  threadDelay 5000000  -- 5 seconds
  return (ph, [outThread, errThread], logFile)

stopServices :: Bool -> (ProcessHandle, [ThreadId], Handle) -> IO ()
stopServices _ (ph, threads, logFile) = do
  forM_ threads killThread
  terminateProcess ph
  _ <- waitForProcess ph
  hFlush logFile
  hClose logFile
  return ()
