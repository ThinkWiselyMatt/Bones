{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( 
        someFunc,
        tryReadProcess,
        tryCallCommand
    ) where

import qualified Data.Text.IO as T
import System.Process (readProcess, callCommand)
import Control.Exception (catch, SomeException)

someFunc :: IO ()
someFunc = T.putStrLn "From bones to a skeleton"

tryReadProcess :: FilePath -> [String] -> String -> IO (Either String String)
tryReadProcess cmd args input = catch (Right <$> readProcess cmd args input) (return . Left . show :: SomeException -> IO (Either String String))

tryCallCommand :: FilePath -> IO (Either String ())
tryCallCommand cmd = catch (callCommand cmd >> return (Right ())) (return . Left . show :: SomeException -> IO (Either String ()))