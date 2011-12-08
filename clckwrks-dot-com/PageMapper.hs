module PageMapper where

import Clckwrks
import qualified Theme.Page as Page
import qualified Theme.Home as Home

pageMapper :: XMLGenT (Clck ClckURL) XML
pageMapper =
    do pid <- XMLGenT $ getPageId
       case pid of
         (PageId 1) -> Home.page
         _          -> Page.page
