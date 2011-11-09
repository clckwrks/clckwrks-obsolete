{-# LANGUAGE TemplateHaskell #-}
module URL 
     ( SiteURL(..)
     , AuthURL(..)
     , ProfileURL(..)
     , AuthProfileURL(..)
     , ProfileDataURL(..)
     ) where

import Control.Applicative ((<$>))
import Page.Acid       (PageId(..))
import Admin.URL       (AdminURL(..))
import Happstack.Auth  (AuthURL(..), ProfileURL(..), AuthProfileURL(..))
import ProfileData.URL (ProfileDataURL(..))
import Web.Routes
import Web.Routes.TH


data SiteURL
    = ViewPage PageId
    | Admin AdminURL
    | Profile ProfileDataURL
    | Auth AuthProfileURL
      deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)