{-# LANGUAGE TemplateHaskell #-}
module URL where

import Clckwrks.URL   (ClckURL)
import Clckwrks.Bugs  (BugsURL)
import Clckwrks.Media (MediaURL)
import Web.Routes.TH  (derivePathInfo)

data SiteURL
    = C ClckURL
    | B BugsURL
    | M MediaURL
$(derivePathInfo ''SiteURL)
