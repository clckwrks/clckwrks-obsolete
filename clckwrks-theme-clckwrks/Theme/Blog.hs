{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Blog where

import Clckwrks
import Data.Text (unpack)
import Theme.Template

postsHTML :: XMLGenT (Clck ClckURL) XML
postsHTML =
    do posts <- getPosts
       <ol class="blog-posts">
        <% mapM postHTML posts %>
        </ol>

postHTML :: Page -> XMLGenT (Clck ClckURL) XML
postHTML Page{..} =
    <li class="blog-post">
     <h2><% pageTitle %></h2>
     <span class="pub-date"><% pageDate %></span>
     <% pageSrc %>
     <p><a href=(ViewPage pageId)>permalink</a></p>
    </li>

page :: XMLGenT (Clck ClckURL) XML
page =
    do ttl <- lift getBlogTitle
       template (unpack ttl) () $
           <%>
            <div id="blog-content">
             <h1 class="page-title"><% ttl %></h1>
             <% postsHTML %>
            </div>
           </%>
