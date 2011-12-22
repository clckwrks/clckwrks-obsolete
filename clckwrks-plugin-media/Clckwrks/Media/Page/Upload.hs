{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Media.Page.Upload where

import Control.Applicative  ((<$>), (<*))
import Control.Monad.Reader (ask)
import Control.Monad.Trans  (liftIO)
import Clckwrks             (update, seeOtherURL)
import Clckwrks.Admin.Template (template)
import Clckwrks.FormPart (FormDF, fieldset, ol, li, inputTextArea, multiFormPart)
import Clckwrks.Media.Acid  (GenMediaId(..), PutMedia(..))
import Clckwrks.Media.Monad (MediaConfig(..), MediaT)
import Clckwrks.Media.Types (Media(..), MediaId(..), MediaKind(..))
import Clckwrks.Media.URL   (MediaURL(..))
import           Data.Map   (Map)
import qualified Data.Map   as Map
import Happstack.Server     (Response, ok, setResponseCode, toResponse)
import HSP
import Magic (magicFile)
import Text.Digestive ((++>))
import Text.Digestive.HSP.Html4 (inputFile, label, submit)
import System.Directory (copyFile)
import System.FilePath  ((</>), addExtension, takeExtension)
import Web.Routes (showURL)

extensionMap :: Map String (String, MediaKind)
extensionMap =
    Map.fromList 
           [ ("image/jpeg", ("jpg", JPEG))
           ]

acceptedTypes :: [String]
acceptedTypes = Map.keys extensionMap 

contentTypeExtension :: String -> Maybe (String, MediaKind)
contentTypeExtension ct = Map.lookup (takeWhile (/= ';') ct) extensionMap

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
                       mid@(MediaId i) <- update GenMediaId
                       let destPath = (md </> show i) `addExtension` ext
                       liftIO $ copyFile tempPath destPath
                       let media = Media { mediaId    = mid
                                         , uploadName = origName
                                         , mediaPath  = destPath
                                         , mediaKind  = kind
                                         }
                       update (PutMedia media)
                       seeOtherURL (GetMedia mid)

uploadForm :: FormDF (MediaT IO) (Maybe (String, FilePath))
uploadForm =
    label "upload file: " ++> inputFile <* submit "Upload"
