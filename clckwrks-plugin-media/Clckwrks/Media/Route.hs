module Clckwrks.Media.Route where

import Control.Applicative           ((<$>))
import Control.Monad.Reader          (ask)
import Clckwrks                      (Clck, Role(..), requiresRole_)
import Clckwrks.Media.Monad          (MediaM, MediaConfig(..))
import Clckwrks.Media.URL            (MediaURL(..), MediaAdminURL(..))
import Clckwrks.Media.Page.AllMedia  (allMedia)
import Clckwrks.Media.Page.GetMedium (getMedium)
import Clckwrks.Media.Page.Preview   (previewMedium)
import Clckwrks.Media.Page.Upload    (uploadMedium)
import qualified Data.Set            as Set
import Happstack.Server              (Response)
import Magic

checkAuth :: MediaURL -> MediaM MediaURL
checkAuth url =
    case url of
      MediaAdmin {} ->
          do showFn <- mediaClckURL <$> ask
             requiresRole_ showFn (Set.singleton Administrator) url
      _ -> return url


routeMedia :: MediaURL -> MediaM Response
routeMedia unsecureURL =
    do url <- checkAuth unsecureURL
       case url of
         (MediaAdmin Upload)   -> uploadMedium url
         (MediaAdmin AllMedia) -> allMedia
         (GetMedium mid) -> getMedium mid
         (Preview mid)   -> previewMedium mid
