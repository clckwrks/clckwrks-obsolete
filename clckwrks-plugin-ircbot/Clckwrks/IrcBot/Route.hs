module Clckwrks.IrcBot.Route where

import Control.Applicative             ((<$>))
import Control.Monad.Reader            (ask)
import Clckwrks                        (Clck, Role(..), requiresRole_)
import Clckwrks.IrcBot.Monad           (IrcBotM, IrcBotConfig(..))
import Clckwrks.IrcBot.Page.IrcLog     (ircLog)
import Clckwrks.IrcBot.Page.IrcLogs    (ircLogs)
import Clckwrks.IrcBot.Page.Reconnect  (ircReconnectPage)
import Clckwrks.IrcBot.Page.Settings   (ircBotSettings)
import Clckwrks.IrcBot.URL             (IrcBotURL(..), IrcBotAdminURL(..))
import qualified Data.Set              as Set
import Happstack.Server                (Response, toResponse, notFound)

checkAuth :: IrcBotURL -> IrcBotM IrcBotURL
checkAuth url =
    case url of
      IrcBotAdmin {} ->
          do showFn <- ircBotClckURL <$> ask
             requiresRole_ showFn (Set.singleton Administrator) url
      _ -> return url


routeIrcBot :: IrcBotURL -> IrcBotM Response
routeIrcBot unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         IrcLogs                       -> ircLogs
         IrcLog fp                     -> ircLog fp
         (IrcBotAdmin IrcBotReconnect) -> ircReconnectPage url
         (IrcBotAdmin IrcBotSettings)  -> ircBotSettings url
