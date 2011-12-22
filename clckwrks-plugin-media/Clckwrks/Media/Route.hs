module Clckwrks.Media.Route where

import Clckwrks                (Clck)
import Clckwrks.Media.Monad    (MediaT(..), MediaConfig(..))
import Clckwrks.Media.URL      (MediaURL(..))
import Clckwrks.Media.Page.GetMedia (getMedia)
import Clckwrks.Media.Page.Upload   (uploadMedia)
import Happstack.Server        (Response)
import Magic

routeMedia :: MediaURL -> MediaT IO Response
routeMedia url =
    case url of
      Upload         -> uploadMedia url
      (GetMedia mid) -> getMedia mid
    