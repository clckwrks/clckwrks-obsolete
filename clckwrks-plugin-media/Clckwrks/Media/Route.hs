module Clckwrks.Media.Route where

import Clckwrks                (Clck)
import Clckwrks.Media.Monad    (MediaT(..), MediaConfig(..))
import Clckwrks.Media.URL      (MediaURL(..))
import Clckwrks.Media.Page.GetMedium (getMedium)
import Clckwrks.Media.Page.Preview   (previewMedium)
import Clckwrks.Media.Page.Upload    (uploadMedium)
import Happstack.Server        (Response)
import Magic

routeMedia :: MediaURL -> MediaT IO Response
routeMedia url =
    case url of
      Upload          -> uploadMedium url
      (GetMedium mid) -> getMedium mid
      (Preview mid)   -> previewMedium mid
    