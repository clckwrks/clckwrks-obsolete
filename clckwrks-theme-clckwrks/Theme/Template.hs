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
    template' title headers $
         <div id="page-content">
           <% body %>
         </div>

template' ::
    ( EmbedAsChild (Clck ClckURL) headers
    , EmbedAsChild (Clck ClckURL) body
    ) =>
       String
    -> headers
    -> body
    -> XMLGenT (Clck ClckURL) XML
template' title headers body =
    <html>
     <head>
      <title><% title %></title>
      <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
      <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
      <% headers %>
      <% googleAnalytics %>
     </head>
     <body>

      <div class="page-menu">
       <a href="/" id="menu-logo">clckwrks.com</a>
       <div class="menu-inner-div">
        <% getMenu %>
      <% do mu <- getUserId
            case mu of
              Nothing  -> <span id="login-link"><a href=(Auth $ AuthURL A_Login)>Login</a></span>
              (Just _) -> <span id="login-link"><a href=(Auth $ AuthURL A_Logout)>Logout</a></span>
       %>

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
                        else do pid <- lift getPageId
                                <%>
                                 <div><a href=(Auth $ AuthURL A_Login)>login</a></div>
                                 <div><a href=(Admin Console)>admin console</a></div>
                                 <div><a href=(Admin (EditPage pid))>edit this page</a></div>
                                </%>
       %>
       <div id="copyright">Powered by Happstack & Clckwrks. Copyright 2012, SeeReason Partners LLC</div>
      </div>
     </body>
  </html>
