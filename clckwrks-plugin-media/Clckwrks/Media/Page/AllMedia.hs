{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Media.Page.AllMedia where

import Control.Applicative     ((<$>))
import Control.Monad.Reader    (ask)
import Clckwrks                (ClckURL(..), query)
import Clckwrks.Admin.Template (template)
import Clckwrks.Media.Acid     (AllMediumIds(..))
import Clckwrks.Media.Types    (MediumId(..))
import Clckwrks.Media.Monad    (MediaM)
import Clckwrks.Media.URL      (MediaURL(..))
import Data.String             (fromString)
import Happstack.Server        (Response, asContentType, notFound, serveFile, toResponse)
import HSP 

allMedia :: MediaM Response
allMedia =
    do mediumIds <- query AllMediumIds
       template "all  media" <link rel="stylesheet" type="text/css" href=(PluginData (fromString "media") "style.css") /> $ 
                mkGallery mediumIds

mkGallery :: [MediumId] -> XMLGenT MediaM XML
mkGallery mediumIds =
    <ul id="media-gallery">
      <% mapM mkPreview mediumIds %>
    </ul>

mkPreview :: MediumId -> XMLGenT MediaM XML
mkPreview mid =
       <li>
        <a href=(GetMedium mid)><img src=(Preview mid) /></a>
       </li>
