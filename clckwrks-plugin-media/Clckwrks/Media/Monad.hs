{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Clckwrks.Media.Monad where

import Acid
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Clckwrks (ClckT(..))
import Clckwrks.Media.Acid
import Clckwrks.Media.Types
import Clckwrks.Media.URL
import Data.Acid (AcidState)
import Data.Acid.Local (createCheckpointAndClose, openLocalStateFrom)
import Data.Maybe (fromMaybe)
import Magic (Magic, MagicFlag(..), magicLoadDefault, magicOpen)
import System.FilePath ((</>))

data MediaConfig = MediaConfig
    { mediaDirectory :: FilePath -- ^ directory in which to store uploaded media files
    , mediaState     :: AcidState MediaState
    , mediaMagic     :: Magic
    }

type MediaT m = ClckT MediaURL (ReaderT MediaConfig m)

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
