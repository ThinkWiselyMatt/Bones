module TestHelper (startServices, stopServices) where
--dont call this from outside of tests / specs, services get started in Main
import System.IO (openFile, hGetContents, hPutStr, hFlush, hClose, hPutStrLn, stderr, Handle, IOMode(AppendMode))
import System.Process (createProcess, proc, CreateProcess(..), StdStream(CreatePipe), ProcessHandle, terminateProcess, waitForProcess)
import Control.Concurrent (forkIO, ThreadId, killThread, threadDelay)
import Control.Monad (forM_)

startServices :: Bool -> IO (ProcessHandle, [ThreadId], Handle)
startServices logOutput = do
  logFile <- openFile "test_output.log" AppendMode
  (_, Just hout, Just herr, ph) <- createProcess (proc "stack" ["exec", "bones-exe"])
    { std_out = CreatePipe, std_err = CreatePipe }
  
  let handleOutput handle = forkIO $ do
        contents <- hGetContents handle
        putStrLn "Reading output from handle..." -- Debug statement
        if logOutput
          then mapM_ (hPutStrLn stderr) (lines contents)
          else mapM_ (hPutStrLn logFile) (lines contents) >> hFlush logFile
  
  outThread <- handleOutput hout
  errThread <- handleOutput herr

  -- Give the services some time to start
  threadDelay 2000000  -- 2 seconds

  return (ph, [outThread, errThread], logFile)

stopServices :: Bool -> (ProcessHandle, [ThreadId], Handle) -> IO () 
stopServices _ (ph, threads, logFile) = do
  putStrLn "Stopping services..."
  forM_ threads $ \thread -> do
    putStrLn $ "Killing thread: " ++ show thread
    killThread thread
  putStrLn "All threads killed. Terminating process..."
  terminateProcess ph
  putStrLn "Waiting for process to terminate..."
  _ <- waitForProcess ph
  putStrLn "Process terminated. Flushing and closing log file..."
  hFlush logFile
  hClose logFile
  putStrLn "Services stopped."
  return ()