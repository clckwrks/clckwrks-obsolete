module Admin.Route where

import Admin.Console 
import Admin.URL
import Admin.EditPage
import Admin.NewPage
import CMS
import Menu.Acid
import Menu.Edit

routeAdmin :: AdminURL -> CMS CMSURL Response
routeAdmin url =
    case url of
      Console        -> nestURL Admin $ consolePage
      (EditPage pid) -> editPage (Admin url) pid
      NewPage        -> nestURL Admin $ newPage
      EditMenu       -> 
          do menu <- query AskMenu
             editMenu menu
