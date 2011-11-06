module Admin.Route where

import Admin.Console 
import Admin.URL
import Admin.EditPage
import Admin.NewPage
import CMS

routeAdmin :: AdminURL -> CMS Response
routeAdmin url =
    case url of
      Console        -> consolePage
      (EditPage pid) -> editPage (Admin url) pid
      NewPage        -> newPage
