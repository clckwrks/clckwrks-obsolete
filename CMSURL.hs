{-# LANGUAGE TemplateHaskell #-}
module CMSURL where

import Control.Applicative ((<$>))
import Page.Acid (PageId(..))
import Admin.URL
import Web.Routes
import Web.Routes.TH


data CMSURL 
    = ViewPage PageId
    | Admin AdminURL
      deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''CMSURL)