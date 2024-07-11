{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheckSpecs (spec) where

import Test.Hspec
import Test.QuickCheck
import Network.HTTP.Client (newManager, defaultManagerSettings, httpLbs, parseRequest, responseBody, Manager)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)
import Data.List (isPrefixOf, find, isInfixOf, stripPrefix)
import Data.Char (isSpace)


spec :: Spec
spec = describe "QuickCheck tests for all endpoints" $ do
    it "quickcheck tests for all CSharp endpoints" $ \_ -> do
      manager <- newManager defaultManagerSettings
      responses <- retry 3 $ mapM (makeRequest manager) csharpEndpoints
      all (S8.isInfixOf "Hello from C#" . L8.toStrict) responses `shouldBe` True

    it "quickcheck tests for all non CSharp endpoints" $ \_ -> do
      manager <- newManager defaultManagerSettings
      responses <- retry 3 $ mapM (makeRequest manager) baseEndpoints
      all (S8.isInfixOf "Hello from C#" . L8.toStrict) responses `shouldBe` False

    it "quickcheck tests for /cpp/add/x/y endpoints" $ withMaxSuccess 5 $ property $ \x y -> do
      manager <- newManager defaultManagerSettings
      results <- mapM (testCppAddEndpoint manager x y) baseEndpoints
      all id results `shouldBe` True
    
    it "Service still running if you want to test anything in browser -- hit enter to continue" $ \_ -> do
      (1 + 1) `shouldBe` 2

contains :: String -> String -> Bool
contains = isInfixOf

testCppAddEndpoint :: Manager -> Int -> Int -> String -> IO Bool
testCppAddEndpoint manager x y url = do
  let fullUrl = url ++ "/cpp/add/" ++ show x ++ "/" ++ show y
  putStrLn $ "Calling URL: " ++ fullUrl
  request <- parseRequest fullUrl
  response <- httpLbs request manager
  let body = L8.unpack (responseBody response)
  putStrLn $ "Response: " ++ body
  let sumResult = parseSum body
  putStrLn $ "Parsed result: " ++ show sumResult
  return $ case sumResult of
    Just result -> result == x + y
    Nothing -> False

parseSum :: String -> Maybe Int
parseSum body = readMaybe . reverse . takeWhile (/= '=') . dropWhile isSpace . reverse $ body --works backwards dropping whitespace, until it gets to equals sign to just grab the num

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

arbitraryValue :: Gen Int
arbitraryValue = choose (-10000, 10000)

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
