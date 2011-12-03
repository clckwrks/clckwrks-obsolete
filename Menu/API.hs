{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Menu.API where

import CMSMonad
import Data.Text (Text)
import Data.Tree
import HSP hiding (escape)
import Menu.Types
import Menu.Acid
import Types
import URL

mkMenuName :: Text -> CMS url MenuName
mkMenuName name =
    do p <- getPrefix
       u <- getUnique
       return $ MenuName { menuPrefix = p
                         , menuTag    = name
                         , menuUnique = u
                         }

getMenu :: GenXML (CMS CMSURL)
getMenu =
    do menu <- query AskMenu
       menuForestHTML $ menuItems menu

menuForestHTML :: Forest (MenuItem url) -> GenXML (CMS url)
menuForestHTML [] = return $ cdata ""
menuForestHTML forest =
    <ol class="menu">
     <% mapM menuTreeHTML forest %>
    </ol>

menuTreeHTML :: Tree (MenuItem url) -> GenXML (CMS url)
menuTreeHTML (Node menuItem subMenus) =
    <li>
     <a><% menuTitle menuItem %></a>
     <% menuForestHTML subMenus %>
    </li>

             