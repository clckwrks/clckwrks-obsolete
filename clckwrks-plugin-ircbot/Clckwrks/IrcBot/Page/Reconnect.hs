{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.Reconnect where

import Control.Applicative      ((<$>), (<*))
import Control.Monad.Reader     (ask)
import Control.Monad.Trans      (liftIO)
import Clckwrks                 (update, seeOtherURL)
import Clckwrks.Admin.Template  (template)
import Clckwrks.FormPart        (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Clckwrks.IrcBot.Monad    (IrcBotM(..), IrcBotConfig(..))
import Clckwrks.IrcBot.URL      (IrcBotURL)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import Happstack.Server         (Response, ok, setResponseCode, toResponse)
import HSP
import Text.Digestive           ((++>))
import Text.Digestive.HSP.Html4 (inputFile, label, submit)
import Web.Routes               (showURL)

ircReconnectPage :: IrcBotURL -> IrcBotM Response
ircReconnectPage here =
    do action <- showURL here
       template "Force Reconnect" () $
                <%>
                 <% multiFormPart "ir" action forceReconnect Nothing forceReconnectForm %>
                </%>
    where
      forceReconnect :: String -> IrcBotM Response
      forceReconnect _ =
          do fr <- ircReconnect <$> ask
             liftIO $ putStrLn "attempting reconnect"
             liftIO $ fr
             template "forced reconnect" () $
                      <p>forced bot to reconnect</p>

forceReconnectForm :: FormDF IrcBotM String
forceReconnectForm =
    submit "Force Reconnect"
