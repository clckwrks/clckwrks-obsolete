module Admin.Route where

import Admin.Console 
import Admin.URL
import Admin.EditPage
import Admin.NewPage
import CMS

routeAdmin :: AdminURL -> CMS SiteURL Response
routeAdmin url =
    case url of
      Console        -> nestURL Admin $ consolePage
      (EditPage pid) -> editPage (Admin url) pid
      NewPage        -> nestURL Admin $ newPage
