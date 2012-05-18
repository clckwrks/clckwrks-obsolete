{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.Reconnect where

import Control.Applicative      ((<$>), (<*))
import Control.Monad.Reader     (ask)
import Control.Monad.Trans      (liftIO)
import Clckwrks                 (update, seeOtherURL)
import Clckwrks.Admin.Template  (template)
import Clckwrks.IrcBot.Monad    (IrcBotM(..), IrcBotForm, IrcBotConfig(..))
import Clckwrks.IrcBot.URL      (IrcBotURL)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import Happstack.Server         (Response, ok, setResponseCode, toResponse)
import HSP
import Text.Reform              ((++>))
import Text.Reform.Happstack    (reform)
import Text.Reform.HSP.String   (inputSubmit, form)
import Web.Routes               (showURL)

ircReconnectPage :: IrcBotURL -> IrcBotM Response
ircReconnectPage here =
    do action <- showURL here
       template "Force Reconnect" () $
                <%>
                 <% reform (form action) "ir" forceReconnect Nothing forceReconnectForm %>
                </%>
    where
      forceReconnect :: Maybe String -> IrcBotM Response
      forceReconnect _ =
          do fr <- ircReconnect <$> ask
             liftIO $ putStrLn "attempting reconnect"
             liftIO $ fr
             template "forced reconnect" () $
                      <p>forced bot to reconnect</p>

forceReconnectForm :: IrcBotForm (Maybe String)
forceReconnectForm =
    inputSubmit "Force Reconnect"
