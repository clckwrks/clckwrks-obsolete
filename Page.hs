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
  <link type="text/css" href="/static/theme.css" rel="stylesheet" />
 </head>
 <body>
  <% getPageMenu %>
  <h1><% getPageTitle   %></h1>
  <p><%  getPageContent %></p>
  <p><%  getCurrentTime %></p>
  <p><a href=(Admin Console)>admin</a></p>
 </body>
</html>
