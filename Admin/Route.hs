module Admin.Route where

import Admin.Console 
import Admin.URL
import CMS

routeAdmin :: AdminURL -> CMS Response
routeAdmin url =
    case url of
      Console -> consolePage
      (EditPage pid) -> notFound (toResponse ())
