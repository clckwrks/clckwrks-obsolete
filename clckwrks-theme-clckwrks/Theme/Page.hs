{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Page where

import Clckwrks
import Data.Text      (unpack)
import Theme.Template

page :: XMLGenT (Clck ClckURL) XML
page =
    do ttl <- lift getPageTitle
       template (unpack ttl) () $
           <%>
            <h1 class="page-title"><% getPageTitle %></h1>
            <% getPageContent %>
           </%>
