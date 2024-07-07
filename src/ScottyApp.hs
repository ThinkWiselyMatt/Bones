{-# LANGUAGE OverloadedStrings #-}

module ScottyApp (runScottyApp) where -- scottyApp namespace collision so added run prefix

import Web.Scotty
import Data.Text.Lazy (Text)

runScottyApp :: IO ()
runScottyApp = scotty 3001 $ do
    get "/scotty" $ do
        text "Hello from Scotty!"
