{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.Console where

import Admin.URL
import Admin.Template
import CMS
import Data.Text (Text)
import Page.Acid

consolePage :: CMS Response
consolePage =
    do pages <- query PagesSummary
       template "Administration" () $ editList pages

editList ::  [(PageId, Text)] -> XMLGenT CMS XML
editList [] = <p>There are currently no pages.</p>
editList pgs =
    <ul class="plain-list">
     <% mapM editPageLI pgs %>
    </ul>
    where
      editPageLI :: (PageId, Text) -> XMLGenT CMS XML
      editPageLI (pid, ttl) =
          <a href=(Admin $ EditPage pid)><% ttl %></a>
