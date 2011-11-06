module Markdown where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Text.HTML.SanitizeXSS   (sanitizeBalance)
import           System.Exit             (ExitCode(ExitFailure, ExitSuccess))
import           System.IO               (hClose, hGetContents)
import           System.Process          (waitForProcess, runInteractiveProcess)

markdown :: Text -> IO (Either Text Text)
markdown txt =
    do (inh, outh, errh, ph) <- runInteractiveProcess "markdown" ["--html4tags"] Nothing Nothing
       _ <- forkIO $ do T.hPutStr inh txt 
                        hClose inh
       mvOut <- newEmptyMVar
       _ <- forkIO $ do c <- hGetContents outh
                        putMVar mvOut c
       mvErr <- newEmptyMVar
       _ <- forkIO $ do c <- T.hGetContents errh
                        putMVar mvErr c
       ec <- waitForProcess ph
       case ec of
         (ExitFailure _) ->
             do e <- readMVar mvErr
                return (Left e)
         ExitSuccess ->
             do m <- readMVar mvOut
                return (Right (sanitizeBalance (T.pack m)))
