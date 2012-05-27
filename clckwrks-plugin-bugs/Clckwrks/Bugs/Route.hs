module Clckwrks.Bugs.Route where

import Control.Applicative          ((<$>))
import Control.Monad.Reader         (ask)
import Clckwrks                     (Clck, Role(..), requiresRole_)
import Clckwrks.Bugs.Monad          (BugsM, BugsConfig(..))
import Clckwrks.Bugs.URL            (BugsURL(..), BugsAdminURL(..))
import Clckwrks.Bugs.Page.ViewBug   (viewBug)
import qualified Data.Set           as Set
import Happstack.Server             (Response)
import Magic

checkAuth :: BugsURL -> BugsM BugsURL
checkAuth url =
    case url of
{-
      BugsAdmin {} ->
          do showFn <- mediaClckURL <$> ask
             requiresRole_ showFn (Set.singleton Administrator) url
-}
      _ -> return url

routeBugs :: BugsURL -> BugsM Response
routeBugs unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         (ViewBug bid) -> viewBug bid