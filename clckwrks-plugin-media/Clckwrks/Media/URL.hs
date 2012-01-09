{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Media.URL where

import Clckwrks.Media.Types (MediumId(..))
import Web.Routes.TH (derivePathInfo)

data MediaAdminURL
    = Upload
    | AllMedia
$(derivePathInfo ''MediaAdminURL)

data MediaURL
    = GetMedium MediumId
    | Preview MediumId
    | MediaAdmin MediaAdminURL
$(derivePathInfo ''MediaURL)
