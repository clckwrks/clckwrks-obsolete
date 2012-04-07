{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.IrcLog where

import Control.Applicative   ((<$>))
import Control.Monad.Reader  (ask)
import Clckwrks
import Clckwrks.IrcBot.Monad
import Clckwrks.IrcBot.Page.Template (template)
import Data.List (sort)
import Data.Text.IO as T (readFile)
import Happstack.Server.FileServe.BuildingBlocks (isSafePath)
import System.Directory
import System.FilePath
import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML

ircLog :: FilePath -> IrcBotM Response
ircLog logFile =
    if isSafePath (splitDirectories logFile)
    then case takeExtension logFile of
           ".txt" ->
               do logDir <- ircBotLogDirectory <$> ask
                  c      <- liftIO $ T.readFile (logDir </> logFile)
                  template logFile () <pre><% c %></pre>
           ".html" ->
               do logDir <- ircBotLogDirectory <$> ask
                  serveFile (asContentType "text/html; charset=UTF-8") (logDir </> logFile)
           _ -> notFound $ toResponse $ "not sure what to do with " ++ logFile
    else do unauthorized $ toResponse $ "Access Denied to file " ++ logFile
