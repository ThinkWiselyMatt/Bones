{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "From bones to a skeleton"