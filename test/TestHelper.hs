module TestHelper (startServices, stopServices, initializeLogFileMVar) where

import System.IO (openFile, hGetContents, hFlush, hClose, Handle, IOMode(AppendMode), stderr)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(CreatePipe), ProcessHandle, terminateProcess, waitForProcess)
import Control.Concurrent (forkIO, ThreadId, killThread, threadDelay, MVar, newMVar, withMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Data.Text (pack)
import Lib(logMessage)

startServices :: MVar () -> IO ProcessHandle
startServices logMar = do
    (_, Just hout, Just herr, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
        { std_out = CreatePipe, std_err = CreatePipe }

    let handleOutput handle = forkIO $ do
            contents <- hGetContents handle
            mapM_ (logTHelp logMar) (lines contents)

    _  <- handleOutput hout
    _  <- handleOutput herr

    threadDelay 2000000  -- 2 seconds

    return ph


stopServices :: ProcessHandle -> MVar () -> IO ()
stopServices ph logMar = do
  _ <- createProcess (proc "wmic" ["process", "where", "name='bones-exe.exe'", "call", "terminate"])  -- Windows only -- TODO: detect OS and handle differently for Linux, etc.
  return ()

-- Helper function to log messages to a specific log file
logTHelp :: MVar () -> String -> IO ()
logTHelp logMVar msg = withMVar logMVar $ \_ -> logMessage "logs/THelp.txt" msg

-- Initialize the MVar for logging
initializeLogFileMVar :: IO (MVar ())
initializeLogFileMVar = newMVar ()