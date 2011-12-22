{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.Media.Types where

import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), ixSet, ixFun)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text)
import Web.Routes.TH (derivePathInfo)

newtype MediaId = MediaId { unMediaId :: Integer } 
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
$(derivePathInfo ''MediaId)

data MediaKind
    = JPEG
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''MediaKind)

mediaContentType :: MediaKind -> String
mediaContentType JPEG = "image/jpeg; charset=binary"

data Media
    = Media { mediaId     :: MediaId
            , uploadName  :: FilePath
            , mediaPath   :: FilePath
            , mediaKind   :: MediaKind
--            , fileSize    :: Integer
            }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Media)

data PreviewSize
    = Small
    | Medium
    | Large
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''PreviewSize)

instance Indexable Media where
    empty = ixSet [ ixFun ((:[]) . mediaId)
                  ]