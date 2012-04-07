{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.IrcLogs where

import Control.Applicative   ((<$>))
import Control.Monad.Reader  (ask)
import Clckwrks
import Clckwrks.IrcBot.Monad
import Clckwrks.IrcBot.Page.Template (template)
import Clckwrks.IrcBot.URL
import Data.List (sort)
import System.Directory
import System.FilePath
import HSP
import Happstack.Server.HSP.HTML

ircLogs :: IrcBotM Response
ircLogs =
    do logDir   <- ircBotLogDirectory <$> ask
       logFiles <- liftIO $ (reverse . sort . filter ((\ext -> (ext == ".html") || (ext == ".txt")) . takeExtension)) <$> getDirectoryContents logDir
       urls     <- mapM (showURL . IrcLog) logFiles
       let logs = zip urls logFiles
       template "irc logs" ()
         <ul>
           <% mapM (\(url, logFile) -> <li><a href=url><% logFile %></a></li>) logs %>
         </ul>
