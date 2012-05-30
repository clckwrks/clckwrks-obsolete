{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Media.Page.Upload where

import Control.Applicative  ((<$>), (<*))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans  (liftIO)
import Clckwrks             (ClckT, ClckFormT, update, seeOtherURL)
import Clckwrks.Admin.Template (template)
import Clckwrks.Media.Acid  (GenMediumId(..), PutMedium(..))
import Clckwrks.Media.Monad (MediaConfig(..), MediaM, MediaForm)
import Clckwrks.Media.Types (Medium(..), MediumId(..), MediumKind(..))
import Clckwrks.Media.URL   (MediaURL(..))
import           Data.Map   (Map)
import qualified Data.Map   as Map
import Data.Text            (pack)
import Happstack.Server     (ContentType, Input, Response, ServerPartT, ok, setResponseCode, toResponse)
import HSP
import Magic                (magicFile)
import Text.Reform          ((++>), FormError(..))
import Text.Reform.Happstack (reform)
import Text.Reform.HSP.Text (inputFile, label, inputSubmit, textarea, fieldset, ol, li, form)
import System.Directory (copyFile)
import System.FilePath  ((</>), addExtension, takeExtension)
import Web.Routes (showURL)

extensionMap :: Map String (String, MediumKind)
extensionMap =
    Map.fromList
           [ ("image/jpeg", ("jpg", JPEG))
           ]

acceptedTypes :: [String]
acceptedTypes = Map.keys extensionMap

contentTypeExtension :: String -> Maybe (String, MediumKind)
contentTypeExtension ct = Map.lookup (takeWhile (/= ';') ct) extensionMap

uploadMedium :: MediaURL -> MediaM Response
uploadMedium here =
    do template "Upload Medium" () $
        <%>
         <% reform (form here) "ep" saveMedium Nothing uploadForm %>
        </%>

    where
      saveMedium :: (String, FilePath, ContentType) -> MediaM Response
      saveMedium (origName, tempPath, contentType) =
          do md    <- mediaDirectory <$> ask
             magic <- mediaMagic <$> ask
             contentType <- liftIO $ magicFile magic tempPath
             case contentTypeExtension contentType of
                Nothing ->
                    do setResponseCode 415
                       template "Unsupported Type" () $
                           <%>
                            <h1>Unsupported Type</h1>
                            <p>The file you uploaded appears to have the content type <b><% contentType %></b>. However, at this time the only supported types are <b><% acceptedTypes %></b>.</p>
                           </%>
                (Just (ext, kind)) ->
                    do -- renameFile would be faster, but may not work if it has to cross physical devices
                       -- in theory, the filename could be the md5sum of the file making it easy to check for corruption
                       mid@(MediumId i) <- update GenMediumId
                       let destPath = show i `addExtension` ext
                       liftIO $ copyFile tempPath (md </> destPath)
                       let medium = Medium { mediumId   = mid
                                           , uploadName = origName
                                           , mediumPath = destPath
                                           , mediumKind = kind
                                           }
                       update (PutMedium medium)
                       seeOtherURL (GetMedium mid)

uploadForm :: MediaForm (FilePath, FilePath, ContentType)
uploadForm =
    label "upload file: " ++> inputFile <* inputSubmit (pack "Upload")
