{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module WebServiceSpecs (spec) where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent (threadDelay)
import System.Process (createProcess, proc, terminateProcess, waitForProcess, ProcessHandle)
import Data.Text (isInfixOf)
import Data.Text.Encoding (decodeUtf8)

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

startServices :: IO ProcessHandle
startServices = do
  (_, _, _, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
  -- Give the services some time to start
  threadDelay 5000000  -- 5 seconds
  return ph

stopServices :: ProcessHandle -> IO ()
stopServices ph = do
  terminateProcess ph
  _ <- waitForProcess ph
  return ()
