{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import qualified ScottyApp
import qualified YesodApp
import qualified ServantApp
import Control.Concurrent (threadDelay)
import Lib (someFunc)

main :: IO ()
main = do
    -- Output the message from someFunc
    someFunc

    putStrLn "Starting all applications..."
    _ <- forkIO ScottyApp.runScottyApp
    _ <- forkIO YesodApp.yesodApp
    _ <- forkIO ServantApp.servantApp
    putStrLn "Applications are running. Press Ctrl+C to exit."
    --I think getLine was blocking logging threads???
    -- Keep the main thread running indefinitely
    let loop = do
            threadDelay 1000000  -- 1 second
            loop
    loop