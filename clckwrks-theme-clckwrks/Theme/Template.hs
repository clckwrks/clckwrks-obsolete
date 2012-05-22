{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Template where

import Clckwrks
import Clckwrks.ProfileData.Acid (HasRole(..))
import qualified Data.Set        as Set
import Data.String               (IsString(..))
import Data.Text                 (Text)
import HSP.Google.Analytics      (UACCT(..), analyticsAsync)

template ::
    ( EmbedAsChild (Clck ClckURL) headers
    , EmbedAsChild (Clck ClckURL) body
    ) =>
       String
    -> headers
    -> body
    -> XMLGenT (Clck ClckURL) XML
template title headers body =
    <html>
     <head>
      <title><% title %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
      <% headers %>
      -- TODO: only show when running on live site, not testing/devel
      <% analyticsAsync (UACCT "UA-31793881-1") %>
     </head>
     <body>
         <div id="banner-box">
          <div class="mesh"></div>

          <div class="img-text-bg"></div>
          <div class="img-text">The relentless, uncompromised power and beauty of Haskell in a web framework.</div>
          <img src=(ThemeData "clckwrks-logo.png") />
         </div>

      <div class="page-menu">
       <span id="logo">Clckwrks</span>
       <div class="menu-inner-div">
        <% getMenu %>
       </div>
      </div>
      <% body %>

    <div id="footer">
     <% do mu <- getUserId
           case mu of
             Nothing -> <% () %>
             (Just uid) ->
                 do r <- query (HasRole uid (Set.singleton Administrator))
                    if not r
                      then <% () %>
                      else <%>
                            <div><a href=(Auth $ AuthURL A_Login)>login</a></div>
                            <div><a href=(Admin Console)>admin console</a></div>
                           </%>
       %>
     <div id="copyright">Powered by Happstack. Copyright 2012, SeeReason Partners LLC</div>
    </div>
   </body>
  </html>
