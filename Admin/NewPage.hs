{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.NewPage where

import CMS
import Page.Acid as Acid

newPage :: CMS Response
newPage =
    do methodOnly POST
       page <- update Acid.NewPage
       seeOtherURL (Admin $ EditPage (pageId page))
