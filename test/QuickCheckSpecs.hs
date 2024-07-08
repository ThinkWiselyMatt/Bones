{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheckSpecs (spec) where

import Test.Hspec
import Network.HTTP.Client (newManager, defaultManagerSettings, httpLbs, parseRequest, responseBody, Manager)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)

spec :: Spec
spec = beforeAll startServices $ afterAll (const stopServices) $ do
  describe "QuickCheck tests for all endpoints" $ do
    it "quickcheck tests for all CSharp endpoints" $ do
      manager <- newManager defaultManagerSettings
      responses <- retry 3 $ mapM (makeRequest manager) csharpEndpoints
      all (\body -> S8.isInfixOf "Hello from C#" (L8.toStrict body)) responses `shouldBe` True

    it "quickcheck tests for all non CSharp endpoints" $ do
      manager <- newManager defaultManagerSettings
      responses <- retry 3 $ mapM (makeRequest manager) baseEndpoints
      all (\body -> not (S8.isInfixOf "Hello from C#" (L8.toStrict body))) responses `shouldBe` True

startServices :: IO ()
startServices = do
  -- Start your services here (e.g., stack exec bones-exe)
  -- Make sure to wait until services are up and running
  putStrLn "Starting services..."
  -- Simulate starting services
  threadDelay 5000000  -- 5 seconds

stopServices :: IO ()
stopServices = do
  -- Stop your services here
  putStrLn "Stopping services..."

makeRequest :: Manager -> String -> IO L8.ByteString
makeRequest manager url = do
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response

csharpEndpoints :: [String]
csharpEndpoints =
  [ 
    "http://localhost:3001/scotty/csharp"
  , "http://localhost:3002/yesod/csharp"
  , "http://localhost:3003/servant/csharp"
  ]

baseEndpoints :: [String]
baseEndpoints =
  [ "http://localhost:3001/scotty"  
  , "http://localhost:3002/yesod"  
  , "http://localhost:3003/servant"  
  ]

retry :: forall a. Int -> IO a -> IO a
retry n action
  | n <= 0 = action
  | otherwise = do
      result <- try action
      case result of
        Right val -> return val
        Left (_ :: SomeException) -> do
          threadDelay 1000000  -- 1 second
          retry (n - 1) action
