{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Theme.Home where

import Clckwrks
import Theme.Template

summaryBox :: PageId -> String -> String -> GenXML (Clck ClckURL)
summaryBox pid title iconURL =
    <div class="summary-box">
     <h2><% title %></h2>
--     <img src=(ThemeData iconURL) />
     <% getPageSummary pid %>
     <p class="read-more"><a href=(ViewPage pid)>read more...</a></p>
    </div>

page :: XMLGenT (Clck ClckURL) XML
page =
    template' "clckwrks.com" () $
        <%>
         <div id="logo">
          <img src=(ThemeData "clckwrks-logo.png") />
         </div>

         <blockquote>
--          <p><span class="big-quote">“</span>runs smoothly and invisibly<span class="big-quote">”</span> - <span class="quote-author">Katherine Durkes</span></p>
          <p><span class="big-quote">“</span>An open-source CMS you can trust, built with the capability of Haskell.<span class="big-quote">”</span></p>
         </blockquote>

         <div id="page-content">
          <% getPageContent %>
         </div>

         <div class="summary-boxes">
           <% summaryBox (PageId 8) "Why?" "philosophy-icon.png" %>
           <% summaryBox (PageId 3) "Get Started" "7-icon.png" %>
           <% summaryBox (PageId 2) "Get Involved" "8-icon.png" %>
         </div>
        </%>
