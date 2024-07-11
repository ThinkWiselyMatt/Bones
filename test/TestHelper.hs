module TestHelper (startServices, stopServices) where

import System.IO (openFile, hGetContents, hFlush, hClose, Handle, IOMode(AppendMode), stderr)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(CreatePipe), ProcessHandle, terminateProcess, waitForProcess)
import Control.Concurrent (forkIO, ThreadId, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN, LoggingT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Data.Text (pack)

startServices :: Bool -> LoggingT IO (ProcessHandle, [ThreadId], Handle)
startServices logOutput = do
    logFile <- liftIO $ openFile "test_output.log" AppendMode
    (_, Just hout, Just herr, ph) <- liftIO $ createProcess (proc "stack" ["exec", "bones-exe"])
        { std_out = CreatePipe, std_err = CreatePipe }

    let handleOutput handle logToStdErr = forkIO $ do
            contents <- hGetContents handle
            if logToStdErr
                then mapM_ (TIO.hPutStrLn stderr . pack) (lines contents)
                else mapM_ (TIO.hPutStrLn logFile . pack) (lines contents) >> hFlush logFile

    outThread <- liftIO $ handleOutput hout logOutput
    errThread <- liftIO $ handleOutput herr logOutput

    liftIO $ threadDelay 2000000  -- 2 seconds

    return (ph, [outThread, errThread], logFile)

stopServices :: Bool -> (ProcessHandle, [ThreadId], Handle) -> LoggingT IO ()
stopServices _ (ph, threads, logFile) = do
  logInfoN $ pack "Stopping services..."
  liftIO $ terminateProcess ph
  logInfoN $ pack "Waiting for process to terminate..."
  _ <- liftIO $ waitForProcess ph
  logInfoN $ pack "Process terminated. Flushing and closing log file..."
  liftIO $ hFlush logFile
  liftIO $ hClose logFile
  logInfoN $ pack "Services stopped."
  return ()
