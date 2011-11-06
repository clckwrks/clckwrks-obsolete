{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.Console where

import Admin.URL
import Admin.Template
import CMS
import Data.Text (Text)
import Page.Acid (PagesSummary(..))

consolePage :: CMS Response
consolePage =
    do pages <- query PagesSummary
       template "Administration" () $
         <div>
          <form action=(Admin NewPage) method="POST" enctype="multipart/form-data">
           <button type="submit">Create New Page</button>
          </form>
          <% editList pages %>
         </div>

editList ::  [(PageId, Text)] -> GenChildList CMS
editList [] = <%><p>There are currently no pages.</p></%>
editList pgs =
    <%>
     <p>Edit Page</p>
     <ul class="plain-list">
      <% mapM editPageLI pgs %>
     </ul>
    </%>
    where
      editPageLI :: (PageId, Text) -> GenXML CMS
      editPageLI (pid, ttl) =
          <li><a href=(Admin $ EditPage pid)><% ttl %></a></li>
