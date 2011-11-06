{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Page.Types where

import Control.Applicative ((<$>))
import Data.Data
import Data.IxSet
import Data.SafeCopy
import Data.Text
import Web.Routes

instance PathInfo PageId where
    toPathSegments (PageId i) = toPathSegments i
    fromPathSegments = PageId <$> fromPathSegments

newtype PageId = PageId { unPageId :: Integer }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageId)

data PageSrc
    = Markdown { toText :: Text }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageSrc)


data Page 
    = Page { pageId    :: PageId
           , pageTitle :: Text 
           , pageSrc   :: PageSrc
           }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''Page)

instance Indexable Page where
    empty = ixSet [ ixFun ((:[]) . pageId) 
                  ]

type Pages = IxSet Page
