{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Media.Upload where

import Admin.Template
import Clckwrks.Media.URL
import Clckwrks
import FormPart (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Happstack.Server
import HSP
import Text.Digestive
import Text.Digestive.HSP.Html4
import Web.Routes (showURL)

uploadMedia :: MediaURL -> Clck MediaURL Response
uploadMedia here =
    do action <- showURL here
       template "upload media" () $
        <%>
         <% multiFormPart "ep" action saveMedia Nothing uploadForm %>
        </%>
    where
      saveMedia (Just (origName, tempPath)) =
          ok $ toResponse ()

uploadForm :: FormDF (Clck MediaURL) (Maybe (String, FilePath))
uploadForm =
    label "upload file: " ++> inputFile