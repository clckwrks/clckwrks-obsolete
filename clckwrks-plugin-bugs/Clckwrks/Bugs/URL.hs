{-# LANGUAGE TemplateHaskell #-}
module Clckwrks.Bugs.URL where

import Clckwrks.Bugs.Types (BugId(..))
import Web.Routes.TH (derivePathInfo)

data BugsAdminURL
    = EditBug BugId
    | EditMilestones
$(derivePathInfo ''BugsAdminURL)

data BugsURL
    = ViewBug BugId
    | SubmitBug
    | SearchBugs
    | BugsAdmin BugsAdminURL
    | BugsData FilePath
    | Timeline
$(derivePathInfo ''BugsURL)
