{-# LANGUAGE TemplateHaskell #-}
module URL where

import Control.Applicative ((<$>))
import Page.Acid (PageId(..))
import Admin.URL
import Web.Routes
import Web.Routes.TH


data SiteURL
    = ViewPage PageId
    | Admin AdminURL
      deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)