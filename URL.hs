{-# LANGUAGE TemplateHaskell #-}
module URL where

import Control.Applicative ((<$>))
import Page.Acid       (PageId(..))
import Admin.URL       (AdminURL(..))
import ProfileData.URL (ProfileDataURL(..))
import Web.Routes
import Web.Routes.TH


data SiteURL
    = ViewPage PageId
    | Admin AdminURL
    | Profile ProfileDataURL
      deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)