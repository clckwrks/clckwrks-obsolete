{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Page.API 
    ( PageId(..)
    , getPage
    , getPageTitle
    , getPageText
    , getPagesSummary
    , getPageMenu
    ) where

import Acid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import CMSMonad
import CMSURL
import HSP hiding (escape)
import Data.Text
import Happstack.Server
import Page.Acid

getPage :: CMS Page
getPage = 
    do CMSState{..} <- get
       mPage <- query (PageById currentPage)
       case mPage of
         Nothing -> escape $ internalServerError $ toResponse ("getPage: invalid PageId " ++ show (unPageId currentPage))
         (Just p) -> return p

getPageTitle :: CMS Text
getPageTitle = pageTitle <$> getPage

getPageText :: CMS Text
getPageText = (toText . pageSrc) <$> getPage

getPagesSummary :: CMS [(PageId, Text)]
getPagesSummary = query PagesSummary

getPageMenu :: GenXML CMS 
getPageMenu = 
    do ps <- XMLGenT $ query PagesSummary
       case ps of
         [] -> <div>No pages found.</div>
         _ -> <ul class="page-menu">
                <% mapM (\(pid, ttl) -> <li><a href=(ViewPage pid) title=ttl><% ttl %></a></li>) ps %>
              </ul>
    