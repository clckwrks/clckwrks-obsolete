{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
<%
module Theme.Blog where

import Clckwrks

postsHTML :: XMLGenT (Clck ClckURL) XML
postsHTML =
    do posts <- getPosts
       <ol id="blog-posts">
        <% mapM postHTML posts %>
        </ol>

postHTML :: Page -> XMLGenT (Clck ClckURL) XML
postHTML Page{..} =
    <li class="blog-post">
     <h1><% pageTitle %></h1>
     <span class="pub-date"><% pageDate %></span>
     <% pageSrc %>
     <p><a href=(ViewPage pageId)>permalink</a></p>
    </li>

page :: XMLGenT (Clck ClckURL) XML

%>

<html>
 <head>
  <title>Blog</title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css")    />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>
  <div id="clckwrks-menu">
    <span id="clck"><a href="/">Clck</a></span><span id="wrks"><a href="/">wrks</a></span><br />
    <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
    <% getMenu %>
  </div>

  <div id="clckwrks-body">
   <% postsHTML %>
  </div>
 </body>
</html>
