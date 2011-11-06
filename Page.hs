{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Page where
import CMS
import Data.Time.Clock

page :: XMLGenT CMS XML

%>

<html>
 <head>
  <title><% getPageTitle %></title>
 </head>
 <body>
  <h1><% getPageTitle   %></h1>
  <p><%  getPageText    %></p>
  <p><%  getCurrentTime %></p>
  <p>Powered by Awesome!</p>
 </body>
</html>
