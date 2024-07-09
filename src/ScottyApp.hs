{-# LANGUAGE OverloadedStrings #-}

module ScottyApp (runScottyApp) where -- scottyApp namespace collision so added run prefix

import Web.Scotty
import CppFFI
import Pyfi (python, py)
import Foreign.C.String (peekCString)
import Data.Text.Lazy (pack)
import Control.Monad.IO.Class (liftIO)
import Lib (tryReadProcess)

runScottyApp :: IO ()
runScottyApp = scotty 3001 $ do
    get "/scotty" $ do
        text "Hello from Scotty!"

    get "/scotty/csharp" $ do
        let exePath = "ServerDependancies\\CSharpHelloWorld\\HelloWorldLibrary.exe"
        result <- liftIO $ tryReadProcess exePath [] ""
        case result of
            Left err -> text (pack $ "Scotty: " ++ err)
            Right output -> text (pack $ "Scotty: " ++ output)

    get "/scotty/cpp" $ do
      msg <- liftIO (getMessagee >>= peekCString)
      text $ pack $ "Scotty: " ++ msg
      
    get "/scotty/cpp/add/:x/:y" $ do
        x <- captureParam "x"
        y <- captureParam "y"
        sumResult <- liftIO $ add (read x) (read y)
        text $ "Scotty: Sum = " <> (pack . show $ sumResult)
    
    get "/scotty/python/add/:x/:y" $ do
        x <- param "x"
        y <- param "y"
        result <- liftIO $ python "python" $ py "addTwoNumbers" x y
        text $ "Scotty: Sum from Python = " <> (pack . show $ (result :: Int))

    get "/scotty/python/print/:message" $ do
        message <- param "message"
        liftIO $ python "python" $ py "printMessage" message
        text $ "Scotty: Message printed by Python : "



