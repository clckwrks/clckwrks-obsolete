{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Media.URL where

import Clckwrks.Media.Types (MediumId(..))
import Web.Routes.TH (derivePathInfo)

data MediaURL
    = GetMedium MediumId
    | Upload
    | Preview MediumId
    | AllMedia
$(derivePathInfo ''MediaURL)
