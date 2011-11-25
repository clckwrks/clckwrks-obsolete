{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Menu.Edit where

import Admin.Template
import CMSMonad
import Data.Aeson
import Data.String
import Data.Tree
import           Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server
import HSP
import Language.Javascript.JMacro
import Menu.API
import Menu.Types
import Menu.Acid
import Page.Acid
import Types
import URL

editMenu :: Menu url -> CMS url Response
editMenu menu =
    do summaries <- query PagesSummary
       template "edit menu" (headers summaries) $
         <%>
          <button id="create">Create</button>
          <button id="serialize">Serialize</button>
          <div id="menu">
          </div>
         </%>
    where
      headers summaries
              = <%>
                 <script type="text/javascript" src="/jstree/jquery.jstree.js" ></script>
                 <% [$jmacro|
                      $(document).ready(function () {
                        $("#menu").jstree(`(jstree menu)`);
                        `(serialize)`;
                        `(addPageMenu summaries)`;
                      });
                    |]
                  %>
                </%>

addPageMenu :: [(PageId, Text)] -> JStat
addPageMenu pageSummaries =
    [$jmacro|
      var menu = $.jstree._reference("#menu");

      $("#create").click(function () {
 //       var menu = $.jstree._reference("#menu");
//        console.log(menu);
//      menu.create("#menu", 0, `(root)`, false, true);
        menu.create(null, 0, `(data_)`, false, true);
      });
    |]
    where
      root =
          let (PageId pid, ttl) = head pageSummaries
          in
          object [ fromString "data" .=
                     object [ fromString "title" .= "menu"
                            ]
                 , fromString "attr" .= 
                     object [ fromString "rel" .= "root" 
                            ]
                 ]

      data_ =
          let (PageId pid, ttl) = head pageSummaries
          in
          object [ fromString "data" .=
                     object [ fromString "title" .= ttl
                            ]
                 , fromString "attr" .= 
                     object [ fromString "rel" .= "target" 
                            ]
                 , fromString "metadata"  .= object [ fromString "pid" .= pid ]
                 ]

serialize :: JStat
serialize =
    [$jmacro|
     $("#serialize").click(function () {
       console.log($("#menu").jstree("get_json", -1));
       console.log(JSON.stringify($("#menu").jstree("get_json", -1)));
     });
    |]

jstree :: Menu url -> Value
jstree menu =
    object [ fromString "types" .=
               object [ fromString "types" .=
                         object [ fromString "root" .= 
                                    object [ fromString "max_children" .= (-1 :: Int)
                                           ]
                                , fromString "menu" .= 
                                    object [ fromString "max_children" .= (-1 :: Int)
                                           ]
                                , fromString "target" .= 
                                    object [ fromString "max_children" .= (0 :: Int)
                                           ]
                                ]
                      ]
           , fromString "json_data" .= rootNode (menuToJSTree menu)
           , fromString "plugins"   .= toJSON [ "themes", "ui", "crrm", "types", "json_data" ]
           ]

rootNode :: Value -> Value
rootNode children =
    object  [ fromString "data" .= 
                object [ fromString "data" .= 
                         [ object [ fromString "title" .= "menu"
                                  ]
                         ]
                       ]
            , fromString "children" .= children
            ]

    

menuToJSTree :: Menu url -> Value
menuToJSTree (Menu items) =
    object  [ fromString "data" .= (toJSON $ map menuTreeToJSTree items) 
            ]

menuTreeToJSTree :: Tree (MenuItem url) -> Value
menuTreeToJSTree (Node item children) =
    object [ fromString "data" .= 
               object [ fromString "title" .= menuTitle item ]
           , fromString "children" .=
               map menuTreeToJSTree children
           ]

create :: JStat
create =
    [$jmacro|
      $("#create").click(function () {
         $("#menu").jstree("create", null, 0, `(data_)`, false, true);
       });
    |]
    where
      data_ = 
          object [ fromString "data" .= 
                   object [ fromString "title" .= "foo"
                          ]
--                 , fromString "metadata"  .= object [ fromString "foo" .= "bar" ]
                 ]
