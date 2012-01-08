{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Theme.Page where

import Clckwrks

page :: XMLGenT (Clck ClckURL) XML

%>

<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css")    />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>
  <div id="clckwrks-menu">
    <span id="clck"><a href="/">Clck</a></span><span id="wrks"><a href="/">wrks</a></span><br />
    <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
--    <% getPageMenu %>
    <% getMenu %>
  </div>

  <div id="clckwrks-body">
   <h1><% getPageTitle   %></h1>
   <p><%  getPageContent %></p>

   <p><a href=(Admin Console)>admin</a></p>
   <p><a href=(Auth $ AuthURL A_Login)>login</a></p>

  </div>
 </body>
</html>
