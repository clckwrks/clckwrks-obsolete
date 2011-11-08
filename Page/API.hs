{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Page.API 
    ( PageId(..)
    , getPage
    , getPageTitle
    , getPageContent
    , getPagesSummary
    , getPageMenu
    ) where

import Acid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import CMSMonad
import Data.Text
import Happstack.Server
import HSP hiding (escape)
import Markdown
import Page.Acid
import URL

getPage :: CMS url Page
getPage = 
    do CMSState{..} <- get
       mPage <- query (PageById currentPage)
       case mPage of
         Nothing -> escape $ internalServerError $ toResponse ("getPage: invalid PageId " ++ show (unPageId currentPage))
         (Just p) -> return p

getPageTitle :: CMS url Text
getPageTitle = pageTitle <$> getPage

getPageContent :: CMS url Content
getPageContent = 
    do src <- (toText . pageSrc) <$> getPage
       e <- markdown src
       case e of
         (Left e)     -> return $ PlainText e
         (Right html) -> return $ TrustedHtml html

getPagesSummary :: CMS url [(PageId, Text)]
getPagesSummary = query PagesSummary

getPageMenu :: GenXML (CMS SiteURL)
getPageMenu = 
    do ps <- XMLGenT $ query PagesSummary
       case ps of
         [] -> <div>No pages found.</div>
         _ -> <ul class="page-menu">
                <% mapM (\(pid, ttl) -> <li><a href=(ViewPage pid) title=ttl><% ttl %></a></li>) ps %>
              </ul>
