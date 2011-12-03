{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module URL 
     ( CMSURL(..)
     , AuthURL(..)
     , ProfileURL(..)
     , AuthProfileURL(..)
     , ProfileDataURL(..)
     ) where

import Control.Applicative ((<$>))
import Data.Data       (Data, Typeable)
import Data.SafeCopy   (SafeCopy(..), base, deriveSafeCopy)
import Page.Acid       (PageId(..))
import Admin.URL       (AdminURL(..))
import Happstack.Auth  (AuthURL(..), ProfileURL(..), AuthProfileURL(..))
import Happstack.Auth.Core.AuthURL
import ProfileData.URL (ProfileDataURL(..))
import Web.Routes
import Web.Routes.TH


data CMSURL
    = ViewPage PageId
    | Admin AdminURL
    | Profile ProfileDataURL
    | Auth AuthProfileURL
      deriving (Eq, Ord, Data, Typeable, Read, Show)

$(deriveSafeCopy 1 'base ''AuthURL)
$(deriveSafeCopy 1 'base ''ProfileURL)
$(deriveSafeCopy 1 'base ''AuthProfileURL)
$(deriveSafeCopy 1 'base ''CMSURL)
$(deriveSafeCopy 1 'base ''OpenIdURL)
$(deriveSafeCopy 1 'base ''AuthMode)
$(deriveSafeCopy 1 'base ''OpenIdProvider)

$(derivePathInfo ''CMSURL)