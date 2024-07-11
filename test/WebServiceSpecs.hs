{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module WebServiceSpecs (spec) where

import Test.Hspec
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (isInfixOf)
import Data.Text.Encoding (decodeUtf8)

spec :: Spec
spec = describe "Web Service Apps" $ do
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
    
    it "responds from Scotty C++"  $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3001/scotty/cpp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Scotty:"
      body `shouldContain` "Hello from C++!"

    it "responds from Scotty C++ add"  $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3001/scotty/cpp/add/7/51"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Scotty:"
      body `shouldContain` "Sum = 58"

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

    it "responds from Yesod C++"  $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3002/yesod/cpp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Yesod:"
      body `shouldContain` "Hello from C++!"

    it "responds from Yesod C++ add"  $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3002/yesod/cpp/add/1/51"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Yesod:"
      body `shouldContain` "Sum = 52"

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
    
    it "responds from Servant app C++" $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3003/servant/cpp"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Servant:"
      body `shouldContain` "Hello from C++!"
    
    it "responds from Servant app C++ add" $ \_ -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest "http://localhost:3003/servant/cpp/add/40/2"
      response <- httpLbs request manager
      let body = L8.unpack (responseBody response)
      body `shouldContain` "Servant:"
      body `shouldContain` "Sum = 42"
--continues on to quickcheck specs 

