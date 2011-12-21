{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Clckwrks.Media.Monad where

import Clckwrks (ClckT(..), ClckState, mapClckT)
import Clckwrks.Acid
import Clckwrks.Media.Acid
import Clckwrks.Media.URL
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Data.Acid (AcidState)
import Data.Acid.Local (createCheckpointAndClose, openLocalStateFrom)
import Data.Maybe (fromMaybe)
import Happstack.Server
import Happstack.Server.Internal.Monads (FilterFun)
import Magic (Magic, MagicFlag(..), magicLoadDefault, magicOpen)
import System.FilePath ((</>))

data MediaConfig = MediaConfig
    { mediaDirectory :: FilePath -- ^ directory in which to store uploaded media files
    , mediaState     :: AcidState MediaState
    , mediaMagic     :: Magic
    }

type MediaT m = ClckT MediaURL (ReaderT MediaConfig m)

runMediaT :: MediaConfig -> MediaT m a -> ClckT MediaURL m a
runMediaT mc m = mapClckT f m
    where
      f :: ReaderT MediaConfig m (Maybe (Either Response a, FilterFun Response), ClckState) 
        -> m (Maybe (Either Response a, FilterFun Response), ClckState)
      f r = runReaderT r mc

instance (Monad m) => MonadReader MediaConfig (MediaT m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (MediaT m) MediaState where
    getAcidState =
        mediaState <$> ask

withMediaConfig :: Maybe FilePath -> FilePath -> (MediaConfig -> IO a) -> IO a
withMediaConfig mBasePath mediaDir f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "media") initialMediaState) (createCheckpointAndClose) $ \media ->
        do magic <- magicOpen [MagicMime, MagicError]
           magicLoadDefault magic
           f (MediaConfig { mediaDirectory = mediaDir
                          , mediaState     = media
                          , mediaMagic     = magic
                          })
