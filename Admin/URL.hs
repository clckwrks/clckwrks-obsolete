{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Admin.URL where

import Data.Data
import Page.Types
import Web.Routes
import Web.Routes.TH

data AdminURL
    = Console
    | EditPage PageId
    | NewPage
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''AdminURL)