{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Media.URL where

import Clckwrks.Media.Types (MediaId(..))
import Web.Routes.TH (derivePathInfo)

data MediaURL
    = GetMedia MediaId
    | Upload
$(derivePathInfo ''MediaURL)
