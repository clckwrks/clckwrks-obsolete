{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Page where

import CMS
import Data.Time.Clock

page :: XMLGenT (CMS SiteURL) XML

%>

<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href="/static/theme/clckwrks/style.css"  />
  <link rel="stylesheet" type="text/css" href="/static/theme/clckwrks/hscolour.css"  />
 </head>
 <body>
  <div id="clckwrks-menu">
    <span id="clck">Clck</span><span id="wrks">wrks</span><br />
    <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
    <% getPageMenu %>
  </div>

  <div id="clckwrks-body">
   <h1><% getPageTitle   %></h1>
   <p><%  getPageContent %></p>

   <p><a href=(Admin Console)>admin</a></p>
   <p><a href=(Auth $ AuthURL A_Login)>login</a></p>

  </div>
 </body>
</html>
