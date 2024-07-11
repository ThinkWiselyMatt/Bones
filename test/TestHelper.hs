module TestHelper (startServices, stopServices) where

import System.IO (openFile, hGetContents, hFlush, hClose, Handle, IOMode(AppendMode), stderr)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(CreatePipe), ProcessHandle, terminateProcess, waitForProcess)
import Control.Concurrent (forkIO, ThreadId, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Data.Text (pack)
import Lib(logMessage)

startServices :: IO ProcessHandle
startServices = do
    (_, Just hout, Just herr, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
        { std_out = CreatePipe, std_err = CreatePipe }

    let handleOutput handle = forkIO $ do
            contents <- hGetContents handle
            mapM_ (TIO.hPutStrLn stderr . pack) (lines contents)

    logTHelp  <- handleOutput hout
    logTHelp  <- handleOutput herr

    threadDelay 2000000  -- 2 seconds

    return ph


stopServices :: ProcessHandle -> IO ()
stopServices ph = do
  createProcess (proc "wmic" ["process", "where", "name='bones-exe.exe'", "call", "terminate"])  -- Windows only -- TODO: detect OS and handle differently for Linux, etc.
  return ()

-- Helper function to log messages to a specific log file
logTHelp :: String -> IO ()
logTHelp = logMessage "logs/THelp.txt"