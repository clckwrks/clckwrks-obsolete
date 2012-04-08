{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, OverloadedStrings, RecordWildCards, RankNTypes, FlexibleContexts #-}
module Clckwrks.IrcBot.Monad where

import Clckwrks                     (Clck(..), ClckT(..), ClckState(..), ClckURL(..), mapClckT, addAdminMenu)
import Clckwrks.Acid
import Clckwrks.IrcBot.Acid
import Clckwrks.IrcBot.PreProcess   (ircBotCmd)
import Clckwrks.IrcBot.Types
import Clckwrks.IrcBot.URL
import Control.Applicative           ((<$>))
import Control.Concurrent            (ThreadId, killThread)
import Control.Exception             (bracket, finally)
import Control.Monad.Reader          (ReaderT(..), MonadReader(..))
import Data.Acid                     (AcidState, query)
import Data.Acid.Local               (createCheckpointAndClose, openLocalStateFrom)
import qualified Data.Map            as Map
import Data.Maybe                    (fromMaybe)
import Data.Set                      (Set, insert)
import qualified Data.Text           as T
import Happstack.Server              (ServerPartT)
import Happstack.Server.Internal.Monads (FilterFun)
import HSP                           (Attr((:=)), Attribute(MkAttr), EmbedAsChild(..), EmbedAsAttr(..), IsName(toName), XMLGenT(..), pAttrVal, XML)
import Network                       (PortID(PortNumber))
import Network.IRC.Bot.BotMonad      (BotMonad(..))
import Network.IRC.Bot.Core          (BotConf(..), User(..), nullBotConf, simpleBot)
import Network.IRC.Bot.Log           (LogLevel(..), nullLogger, stdoutLogger)
import Network.IRC.Bot.Part.Dice     (dicePart)
import Network.IRC.Bot.Part.Hello    (helloPart)
import Network.IRC.Bot.Part.Ping     (pingPart)
import Network.IRC.Bot.Part.NickUser (nickUserPart)
import Network.IRC.Bot.Part.Channels (initChannelsPart)
import Network.IRC.Bot.PosixLogger   (posixLogger)
import System.Directory              (createDirectoryIfMissing)
import System.FilePath               ((</>))
import Web.Routes                    (showURL)

data IrcBotConfig = IrcBotConfig
    { ircBotLogDirectory :: FilePath -- ^ directory in which to store irc logs
    , ircBotState        :: AcidState IrcBotState
    , ircBotClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    , ircBotPageTemplate :: ( EmbedAsChild (Clck ClckURL) headers
                            , EmbedAsChild (Clck ClckURL) body
                            ) =>
                            String
                         -> headers
                         -> body
                         -> XMLGenT (Clck IrcBotURL) XML
    , ircReconnect       :: IO ()
    }

type IrcBotT m = ClckT IrcBotURL (ReaderT IrcBotConfig m)
type IrcBotM   = ClckT IrcBotURL (ReaderT IrcBotConfig (ServerPartT IO))

instance (IsName n) => EmbedAsAttr IrcBotM (Attr n IrcBotURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (IsName n) => EmbedAsAttr IrcBotM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- ircBotClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (T.unpack $ showFn url []))

runIrcBotT :: IrcBotConfig -> IrcBotT m a -> ClckT IrcBotURL m a
runIrcBotT mc m = mapClckT f m
    where
      f r = runReaderT r mc

instance (Monad m) => MonadReader IrcBotConfig (IrcBotT m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (IrcBotT m) IrcBotState where
    getAcidState =
        ircBotState <$> ask

withIrcBotConfig :: Maybe FilePath         -- ^ base path to state dir
                 -> IrcConfig              -- ^ initial 'IrcConfig' to use when creating database for the first time
                 -> (forall headers body.
                     ( EmbedAsChild (Clck ClckURL) headers
                     , EmbedAsChild (Clck ClckURL) body
                     ) =>
                       String
                     -> headers
                     -> body
                     -> XMLGenT (Clck IrcBotURL) XML)
                 -> FilePath               -- ^ directory where irc log files are storted
                 -> (IrcBotConfig -> IO a) -- ^ function that uses the 'IrcBotConfig'
                 -> IO a
withIrcBotConfig mBasePath initIrcConfig pageTemplate' ircBotLogDir f =
    do let basePath = fromMaybe "_state" mBasePath
       bracket (openLocalStateFrom (basePath </> "ircBot") (initialIrcBotState initIrcConfig)) (createCheckpointAndClose) $ \ircBot ->
           do ic@IrcConfig{..} <- query ircBot GetIrcConfig
              let botConf = nullBotConf { channelLogger = Just $ posixLogger (Just ircBotLogDir) "#happs"
                                        , host          = ircHost
                                        , port          = PortNumber $ fromIntegral ircPort
                                        , nick          = ircNick
                                        , commandPrefix = ircCommandPrefix
                                        , user          = ircUser
                                        , channels      = ircChannels
                                        }
              ircParts <- initParts (channels botConf)
              (tids, reconnect) <- simpleBot botConf ircParts
              (f (IrcBotConfig { ircBotLogDirectory = ircBotLogDir
                               , ircBotState        = ircBot
                               , ircBotClckURL      = undefined
                               , ircBotPageTemplate = pageTemplate'
                               , ircReconnect       = reconnect
                               })) `finally` (mapM_ killThread tids)


initParts :: (BotMonad m) =>
             Set String  -- ^ set of channels to join
          -> IO [m ()]
initParts chans =
    do (_, channelsPart) <- initChannelsPart chans
       return [ pingPart
              , nickUserPart
              , channelsPart
              , dicePart
              , helloPart
              ]

addIrcBotAdminMenu :: ClckT IrcBotURL IO ()
addIrcBotAdminMenu =
    do reconnectURL <- showURL (IrcBotAdmin IrcBotReconnect)
       settingsURL  <- showURL (IrcBotAdmin IrcBotSettings)
       addAdminMenu ("IrcBot", [ ("Reconnect", reconnectURL)
                               , ("Settings", settingsURL)
                               ])
