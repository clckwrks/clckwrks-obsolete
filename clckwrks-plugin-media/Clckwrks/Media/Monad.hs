{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Clckwrks.Media.Monad where

import Acid
import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Clckwrks (ClckT(..))
import Clckwrks.Media.Acid
import Clckwrks.Media.Types
import Clckwrks.Media.URL
import Data.Acid
import Magic (Magic)

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
