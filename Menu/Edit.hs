{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Menu.Edit where

import Admin.Template
import CMSMonad
import Data.Tree
import Happstack.Server
import HSP
import Language.Javascript.JMacro
import Menu.API
import Menu.Types
import Menu.Acid
import Types
import URL

editMenu :: Menu url -> CMS url Response
editMenu menu =
    template "edit menu" headers $
         <div id="#menu">
         </div>
    where
      headers = <%>
                 <script type="text/javascript" src="/jstree/jquery.jstree.js" ></script>
                 <% [$jmacro|
                      $(document).ready(function () {
                        $("#menu").jstree(`(jstree menu)`);
                      });
                    |]
                  %>
                </%>
