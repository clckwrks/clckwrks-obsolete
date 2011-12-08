module PageMapper where

import Clckwrks
import Page
import qualified Home as Home

pageMapper :: XMLGenT (Clck ClckURL) XML
pageMapper =
    do pid <- XMLGenT $ getPageId
       case pid of
         (PageId 1) -> Home.page
         _          -> page
