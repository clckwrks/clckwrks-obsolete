{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.IrcBot.Page.Settings where

import Control.Applicative      ((<$>), (<*>), (<*))
import Control.Monad.Reader     (ask)
import Control.Monad.Trans      (liftIO)
import Clckwrks                 (query, update, seeOtherURL)
import Clckwrks.Admin.Template  (template)
import Clckwrks.FormPart        (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Clckwrks.IrcBot.Acid     (GetIrcConfig(..), SetIrcConfig(..))
import Clckwrks.IrcBot.Monad    (IrcBotM(..), IrcBotConfig(..))
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
import Text.Digestive           (Transformer, (++>), mapView, transform, transformEither)
import Text.Digestive.HSP.Html4 (inputString, label, submit)
import Web.Routes               (showURL)

ircBotSettings :: IrcBotURL -> IrcBotM Response
ircBotSettings here =
    do action    <- showURL here
       oldConfig <- query GetIrcConfig
       template "IrcBot Settings" () $
                <%>
                 <% multiFormPart "ibs" action updateSettings Nothing (ircBotSettingsForm oldConfig) %>
                </%>
    where
      updateSettings :: IrcConfig -> IrcBotM Response
      updateSettings newConfig =
          do update (SetIrcConfig newConfig)
             template "IrcConfig updated" () $
                      <p>IrcConfig updated</p>

ircBotSettingsForm :: IrcConfig -> FormDF IrcBotM IrcConfig
ircBotSettingsForm IrcConfig{..} =
     ul ((IrcConfig <$> host <*> port <*> nick <*> cp <*> user <*> channels) <* submit "update")
    where
      host     = li $ label "irc server" ++> inputString (Just ircHost)
      port     = li $ label "irc port"   ++> inputString (Just $ show ircPort) `transform` toWord16
      nick     = li $ label "nickname"   ++> inputString (Just ircNick)
      cp       = li $ label "cmd prefix" ++> inputString (Just ircCommandPrefix)
      usrnm    = li $ label "username"   ++> inputString (Just $ username ircUser)
      hstnm    = li $ label "hostname"   ++> inputString (Just $ hostname ircUser)
      srvrnm   = li $ label "servername" ++> inputString (Just $ servername ircUser)
      rlnm     = li $ label "realname"   ++> inputString (Just $ realname ircUser)
      user     = User <$> usrnm <*> hstnm <*> srvrnm <*> rlnm
      channels = li $ label "channels (comma separated)"   ++> inputString (Just $ fromSet ircChannels) `transform` toSet

      -- markup
      li :: FormDF IrcBotM a -> FormDF IrcBotM a
      li = mapView (\xml -> [<li><% xml %></li>])
      ul :: FormDF IrcBotM a -> FormDF IrcBotM a
      ul = mapView (\xml -> [<ul><% xml %></ul>])

      -- transformers
      toWord16 :: Transformer IrcBotM String String Word16
      toWord16 = transformEither $ \str ->
          case readDec str of
            [(n,[])] -> Right n
            _        -> (Left $ "Could not parse as Word16: " ++ str)

      fromSet :: Set String -> String
      fromSet s = intercalate "," (Set.toList s)

      toSet :: Transformer IrcBotM String String (Set String)
      toSet = transformEither $ \str ->
              Right (Set.fromList $ words' str)

      words'                   :: String -> [String]
      words' s                 =  case dropWhile isSep s of
                                "" -> []
                                s' -> w : words' s''
                                      where (w, s'') =
                                             break isSep s'
          where
            isSep c = isSpace c || c == ','
