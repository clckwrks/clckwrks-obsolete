{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.Settings where

import Control.Applicative      ((<$>), (<*>), (<*))
import Control.Monad.Reader     (ask)
import Control.Monad.Trans      (liftIO)
import Clckwrks                 (ClckFormT, query, update, seeOtherURL)
import Clckwrks.Admin.Template  (template)
import Clckwrks.IrcBot.Acid     (GetIrcConfig(..), SetIrcConfig(..))
import Clckwrks.IrcBot.Monad    (IrcBotM(..), IrcBotConfig(..), IrcBotForm, IrcFormError(..))
import Clckwrks.IrcBot.Types    (IrcConfig(..))
import Clckwrks.IrcBot.URL      (IrcBotURL)
import Data.Char                (isSpace)
import Data.List                (intercalate)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Set       (Set)
import qualified Data.Set       as Set
import Data.Word                (Word16)
import Happstack.Server         (Response, ok, setResponseCode, toResponse)
import HSP
import Network.IRC.Bot          (User(..))
import Numeric                  (readDec)
import Text.Reform              ((++>), mapView, transformEither)
import Text.Reform.Happstack    (reform)
import Text.Reform.HSP.String   (inputText, label, inputSubmit, form)
import Web.Routes               (showURL)

ircBotSettings :: IrcBotURL -> IrcBotM Response
ircBotSettings here =
    do action    <- showURL here
       oldConfig <- query GetIrcConfig
       template "IrcBot Settings" () $
                <%>
                 <% reform (form action) "ibs" updateSettings Nothing (ircBotSettingsForm oldConfig) %>
                </%>
    where
      updateSettings :: IrcConfig -> IrcBotM Response
      updateSettings newConfig =
          do update (SetIrcConfig newConfig)
             template "IrcConfig updated" () $
                      <p>IrcConfig updated</p>

ircBotSettingsForm :: IrcConfig -> IrcBotForm IrcConfig
ircBotSettingsForm IrcConfig{..} =
     ul ((IrcConfig <$> host <*> port <*> nick <*> cp <*> user <*> channels) <* inputSubmit "update")
    where
      host     = li $ label "irc server" ++> inputText (ircHost)
      port     = li $ label "irc port"   ++> inputText (show ircPort) `transformEither` toWord16
      nick     = li $ label "nickname"   ++> inputText (ircNick)
      cp       = li $ label "cmd prefix" ++> inputText (ircCommandPrefix)
      usrnm    = li $ label "username"   ++> inputText (username ircUser)
      hstnm    = li $ label "hostname"   ++> inputText (hostname ircUser)
      srvrnm   = li $ label "servername" ++> inputText (servername ircUser)
      rlnm     = li $ label "realname"   ++> inputText (realname ircUser)
      user     = User <$> usrnm <*> hstnm <*> srvrnm <*> rlnm
      channels = li $ label "channels (comma separated)"   ++> inputText (fromSet ircChannels) `transformEither` toSet

      -- markup
      li :: IrcBotForm a -> IrcBotForm a
      li = mapView (\xml -> [<li><% xml %></li>])
      ul :: IrcBotForm a -> IrcBotForm a
      ul = mapView (\xml -> [<ul><% xml %></ul>])

      -- transformers
      toWord16 :: String -> Either IrcFormError Word16
      toWord16 str =
          case readDec str of
            [(n,[])] -> Right n
            _        -> (Left (CouldNotParsePort str))

      fromSet :: Set String -> String
      fromSet s = intercalate "," (Set.toList s)

      toSet :: String -> Either IrcFormError (Set String)
      toSet str =
              Right (Set.fromList $ words' str)

      words'                   :: String -> [String]
      words' s                 =  case dropWhile isSep s of
                                "" -> []
                                s' -> w : words' s''
                                      where (w, s'') =
                                             break isSep s'
          where
            isSep c = isSpace c || c == ','
