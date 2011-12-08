{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Home where

import CMS
import Data.Time.Clock

summaryBox :: PageId -> GenXML (CMS ClckURL)
summaryBox pid =
    <div class="summary-box">
     <% getPageSummary pid %>
     <span class="read-more"><a href=(ViewPage pid)>read more...</a></span>
    </div>


page :: XMLGenT (CMS ClckURL) XML

%>

<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>
  <div id="clckwrks-menu">
    <span id="clck">Clck</span><span id="wrks">wrks</span><br />
    <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
    <% getPageMenu %>
--    <% getMenu %>
  </div>
  <div id="clckwrks-body">
   <h1><% getPageTitle   %></h1>

   <div class="bluebox">
    <div class="bluebox-inside">
      <span>I got a big old blue box.</span>
    </div>
   </div>
  
   <div class="the-path-you-follow">
    <% summaryBox (PageId 2) %>
    <% summaryBox (PageId 3) %>
    <% summaryBox (PageId 4) %>
   </div>
{-
   <p><%  getCurrentTime %></p>
   <p><% show <$> whoami %></p>
-}
   <p><a href=(Admin Console)>admin</a></p>
   <p><a href=(Auth $ AuthURL A_Login)>login</a></p>

  </div>
 </body>
</html>
