{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Media.Upload where

import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.Trans  (liftIO)
import Clckwrks             (update)
import Clckwrks.Admin.Template (template)
import Clckwrks.FormPart (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Clckwrks.Media.Acid  (GenMediaId(..))
import Clckwrks.Media.Monad (MediaConfig(..), MediaT)
import Clckwrks.Media.Types (MediaId(..))
import Clckwrks.Media.URL   (MediaURL(..))
import Happstack.Server     (Response, ok, toResponse)
import HSP
import Magic (magicFile)
import Text.Digestive ((++>))
import Text.Digestive.HSP.Html4 (inputFile, label)
import System.Directory (copyFile)
import System.FilePath  ((</>), addExtension, takeExtension)
import Web.Routes (showURL)

uploadMedia :: MediaURL -> MediaT IO Response
uploadMedia here =
    do action <- showURL here
       template "upload media" () $
        <%>
         <% multiFormPart "ep" action saveMedia Nothing uploadForm %>
        </%>
    where
      saveMedia :: Maybe (String, FilePath) -> MediaT IO Response
      saveMedia (Just (origName, tempPath)) =
          do md <- mediaDirectory <$> ask
             mid@(MediaId i) <- update GenMediaId
             magic <- mediaMagic <$> ask
             contentType <- liftIO $ magicFile magic tempPath
             liftIO $ copyFile tempPath ((md </> show i) `addExtension` (takeExtension origName))   -- renameFile would be faster, but may not work if it has to cross physical devices
             
--             let media = Media 
             ok $ toResponse ()

uploadForm :: FormDF (MediaT IO) (Maybe (String, FilePath))
uploadForm =
    label "upload file: " ++> inputFile
