{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import qualified ScottyApp
import qualified YesodApp
import qualified ServantApp
import Lib (someFunc)

main :: IO ()
main = do
    -- Output the message from someFunc
    someFunc

    putStrLn "Starting all applications..."
    _ <- forkIO ScottyApp.runScottyApp
    _ <- forkIO YesodApp.yesodApp
    _ <- forkIO ServantApp.servantApp
    putStrLn "Applications are running. Press Enter to exit."
    _ <- getLine
    return ()