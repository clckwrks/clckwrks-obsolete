{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Clckwrks.Media.Monad where

import Clckwrks            (ClckT(..), ClckState(..), mapClckT)
import Clckwrks.Acid
import Clckwrks.IOThread   (IOThread(..), startIOThread, killIOThread)
import Clckwrks.Media.Acid
import Clckwrks.Media.Preview 
import Clckwrks.Media.PreProcess (mediaCmd)
import Clckwrks.Media.Types
import Clckwrks.Media.URL
import Control.Applicative ((<$>))
import Control.Exception   (bracket)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Data.Acid           (AcidState)
import Data.Acid.Local     (createCheckpointAndClose, openLocalStateFrom)
import qualified Data.Map  as Map
import Data.Maybe          (fromMaybe)
import Happstack.Server
import Happstack.Server.Internal.Monads (FilterFun)
import Magic                (Magic, MagicFlag(..), magicLoadDefault, magicOpen)
import System.Directory     (createDirectoryIfMissing)
import System.FilePath      ((</>))

data MediaConfig = MediaConfig
    { mediaDirectory :: FilePath -- ^ directory in which to store uploaded media files
    , mediaState     :: AcidState MediaState
    , mediaMagic     :: Magic
    , mediaIOThread  :: IOThread (Medium, PreviewSize) FilePath
    }

type MediaT m = ClckT MediaURL (ReaderT MediaConfig m)
type MediaM = ClckT MediaURL (ReaderT MediaConfig (ServerPartT IO))

runMediaT :: MediaConfig -> MediaT m a -> ClckT MediaURL m a
runMediaT mc m = mapClckT f m
    where
      f r = runReaderT r mc

instance (Monad m) => MonadReader MediaConfig (MediaT m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (MediaT m) MediaState where
    getAcidState =
        mediaState <$> ask

-- seems silly that we have to pass ClckState manually here. Should be able to use get/set/modify by now?
withMediaConfig :: Maybe FilePath -> FilePath -> (MediaConfig -> IO a) -> IO a
withMediaConfig mBasePath mediaDir f =
    do let basePath = fromMaybe "_state" mBasePath
           cacheDir  = mediaDir </> "_cache"
       createDirectoryIfMissing True cacheDir
       bracket (openLocalStateFrom (basePath </> "media") initialMediaState) (createCheckpointAndClose) $ \media ->
         bracket (startIOThread (applyTransforms mediaDir cacheDir)) killIOThread $ \ioThread ->
           do magic <- magicOpen [MagicMime, MagicError]
              magicLoadDefault magic
              f (MediaConfig { mediaDirectory = mediaDir
                             , mediaState     = media
                             , mediaMagic     = magic
                             , mediaIOThread  = ioThread
                             })
