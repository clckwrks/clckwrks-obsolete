{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Bugs.URL where

import Clckwrks.Bugs.Types (BugId(..))
import Web.Routes.TH (derivePathInfo)

data BugsAdminURL
    = EditBug BugId
$(derivePathInfo ''BugsAdminURL)

data BugsURL
    = ViewBug BugId
    | SearchBugs
    | BugsAdmin BugsAdminURL
$(derivePathInfo ''BugsURL)
