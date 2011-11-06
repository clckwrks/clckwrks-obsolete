{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Admin.EditPage where

import Admin.URL
import Admin.Template
import Control.Applicative ((<$>), (<*>), (<*))
import CMS
import Data.Text (Text)
import FormPart (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Page.Acid
import Text.Digestive
import Text.Digestive.HSP.Html4 hiding (inputTextArea)

editPage :: CMSURL -> PageId -> CMS Response
editPage here pid =
    do mPage <- query $ PageById pid
       case mPage of
         Nothing -> notFound $ toResponse $ "Page not found" ++ show (unPageId pid)
         (Just page) ->
             do action <- showURL here
                template "edit page" () $ multiFormPart "ep" action updatePage Nothing (pageFormlet page)
    where
      updatePage :: Page -> CMS Response
      updatePage page =
          do update (UpdatePage page)
             seeOtherURL (ViewPage (pageId page)) 

pageFormlet :: Page -> FormDF CMS Page
pageFormlet page =
    (fieldset $
       ol $ (,) <$> ((li $ label "title:") ++> (li $ inputText (Just (pageTitle page)) `setAttrs` ("size" := "80")))
                <*> ((li $ label "body:") ++> (li $ inputTextArea (Just 80) (Just 50) (Just (toText (pageSrc page)))))
                <*  submit "update")
    `transform` (transformEither toPage)
    where
      toPage :: (Text, Text) -> Either e Page
      toPage (ttl, bdy) = Right $
          Page { pageId    = pageId page
               , pageTitle = ttl
               , pageSrc   = Markdown bdy
               }
