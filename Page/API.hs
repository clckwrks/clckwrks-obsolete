{-# LANGUAGE RecordWildCards #-}
module Page.API 
    ( PageId(..)
    , getPage
    , getPageTitle
    , getPageText
    ) where

import Acid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans (MonadIO)
import CMSMonad
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