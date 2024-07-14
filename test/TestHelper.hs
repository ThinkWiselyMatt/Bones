{-# LANGUAGE CPP #-}

module TestHelper (startServices, stopServices, initializeLogFileMVar) where

import System.IO (openFile, hGetContents, hFlush, hClose, Handle, IOMode(AppendMode), stderr)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(CreatePipe), ProcessHandle, terminateProcess, waitForProcess)
import Control.Concurrent (forkIO, ThreadId, killThread, threadDelay, MVar, newMVar, withMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Data.Text (pack)
import Lib(logMessage)
import System.Info (os)

startServices :: MVar () -> IO ProcessHandle
startServices logMar = do
    (_, _, _, ph) <- createProcess (proc "stack" ["exec", "Bones-exe"])

    threadDelay 2000000  -- 2 seconds

    return ph


stopServices :: ProcessHandle -> MVar () -> IO ()
stopServices ph logMVar = do
#if defined(mingw32_HOST_OS)
  -- Windows specific command to terminate the process
  _ <- createProcess (proc "wmic" ["process", "where", "name='bones-exe.exe'", "call", "terminate"])
#else
  -- Non-Windows (Linux/Ubuntu) specific command to terminate the process
  terminateProcess ph
  _ <- waitForProcess ph
#endif
  return ()

-- Helper function to log messages to a specific log file
logTHelp :: MVar () -> String -> IO ()
logTHelp logMVar msg = withMVar logMVar $ \_ -> logMessage "logs/THelp.txt" msg

-- Initialize the MVar for logging
initializeLogFileMVar :: IO (MVar ())
initializeLogFileMVar = newMVar ()