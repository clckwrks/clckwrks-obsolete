module Clckwrks.Admin.Route where

import Clckwrks
import Clckwrks.Admin.Console
import Clckwrks.Admin.URL
import Clckwrks.Admin.EditFeedConfig (editFeedConfig)
import Clckwrks.Admin.EditPage
import Clckwrks.Admin.EditSettings (editSettings)
import Clckwrks.Admin.NewPage
import Clckwrks.Admin.Pages
import Clckwrks.Menu.Acid
import Clckwrks.Menu.Edit
import Clckwrks.Menu.Types

routeAdmin :: AdminURL -> Clck ClckURL Response
routeAdmin url =
    case url of
      Console        -> nestURL Admin $ consolePage
      (EditPage pid) -> editPage (Admin url) pid
      EditFeedConfig -> editFeedConfig (Admin url)
      EditSettings   -> editSettings   (Admin url)
      NewPage        -> nestURL Admin $ newPage PlainPage
      NewPost        -> nestURL Admin $ newPage Post
      Pages          -> nestURL Admin $ pages
      EditMenu       ->
          do menu <- query AskMenu
             editMenu (menu :: Menu ClckURL)
      MenuPOST       -> menuPost

