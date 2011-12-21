{-# LANGUAGE TemplateHaskell #-}
module URL where

import Clckwrks.URL   (ClckURL)
import Clckwrks.Media (MediaURL)
import Web.Routes.TH  (derivePathInfo)

data SiteURL 
    = C ClckURL
    | M MediaURL
$(derivePathInfo ''SiteURL)
