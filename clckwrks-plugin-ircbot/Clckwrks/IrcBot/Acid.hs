{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.IrcBot.Acid where

import Clckwrks.IrcBot.Types  (IrcConfig(..))
import Control.Applicative    ((<$>))
import Control.Monad.Reader   (ask)
import Control.Monad.State    (modify)
import Data.Acid              (Query, Update, makeAcidic)
import Data.Data              (Data, Typeable)
import Data.IxSet             (IxSet, (@=), getOne, empty, toList, updateIx)
import Data.SafeCopy          (base, deriveSafeCopy)


data IrcBotState = IrcBotState
    { ircConfig :: IrcConfig
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''IrcBotState)

initialIrcBotState :: IrcConfig -> IrcBotState
initialIrcBotState initConfig
    = IrcBotState
      { ircConfig = initConfig
      }

getIrcConfig :: Query IrcBotState IrcConfig
getIrcConfig = ircConfig <$> ask

setIrcConfig :: IrcConfig -> Update IrcBotState ()
setIrcConfig newConfig =
    modify $ \s -> s { ircConfig = newConfig }


$(makeAcidic ''IrcBotState
   [ 'getIrcConfig
   , 'setIrcConfig
   ]
 )
