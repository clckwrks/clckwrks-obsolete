module Clckwrks.Media.Route where

import Clckwrks.Media.Monad  (MediaT)
import Clckwrks.Media.URL    (MediaURL(..))
import Clckwrks.Media.Upload (uploadMedia)
import Happstack.Server      (Response)
import Magic

routeMedia :: MediaURL -> MediaT IO Response
routeMedia url =
    case url of
      Upload -> uploadMedia url
