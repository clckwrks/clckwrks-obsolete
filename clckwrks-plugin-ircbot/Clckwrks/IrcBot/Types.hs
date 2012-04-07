{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.IrcBot.Types
    ( IrcConfig(..)
    , User(..)
    ) where

import Data.Data             (Data, Typeable)
import Data.Word             (Word16)
import Data.IxSet            (Indexable(..), ixSet, ixFun)
import Data.SafeCopy         (SafeCopy, base, deriveSafeCopy)
import Data.Set              (Set)
import Data.Text             (Text)
import Network.IRC.Bot.Types (User(..))
import Web.Routes            (PathInfo(..))

data IrcConfig = IrcConfig
    { ircHost          :: String
    , ircPort          :: Word16
    , ircNick          :: String
    , ircCommandPrefix :: String
    , ircUser          :: User
    , ircChannels      :: Set String
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''IrcConfig)
