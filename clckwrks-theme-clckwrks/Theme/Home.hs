{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Theme.Home where

import Clckwrks
import Theme.Template

summaryBox :: PageId -> String -> String -> GenXML (Clck ClckURL)
summaryBox pid title iconURL =
    <div class="summary-box">
     <h2><% title %></h2>
     <img src=(ThemeData iconURL) />
     <% getPageSummary pid %>
     <p class="read-more"><a href=(ViewPage pid)>read more...</a></p>
    </div>

page :: XMLGenT (Clck ClckURL) XML
page =
    template "Home" () $
        <%>

         <blockquote>
          <p>The relentless, uncompromised power and beauty of Haskell in a web framework.</p>
         </blockquote>

         <div class="summary-boxes">
          <% summaryBox (PageId 5) "Happstack Philosophy" "philosophy-icon.png" %>
          <% summaryBox (PageId 6) "Happstack 7 Release Notes" "7-icon.png" %>
          <% summaryBox (PageId 7) "Happstack 8 Roadmap" "8-icon.png" %>

         </div>

       </%>

