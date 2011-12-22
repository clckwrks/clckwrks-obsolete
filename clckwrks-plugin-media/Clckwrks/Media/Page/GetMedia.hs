module Clckwrks.Media.Page.GetMedia where

import Clckwrks             (query)
import Clckwrks.Media.Acid  (GetMediaById(..))
import Clckwrks.Media.Types (MediaId(..), Media(..), mediaContentType)
import Clckwrks.Media.Monad (MediaT)
import Happstack.Server     (Response, asContentType, notFound, serveFile, toResponse)

getMedia :: MediaId -> MediaT IO Response
getMedia mid =
    do mMedia <- query (GetMediaById mid)
       case mMedia of
         Nothing -> do notFound $ toResponse $ "Invalid media id " ++ show (unMediaId mid)
         (Just media) ->
             serveFile (asContentType (mediaContentType $ mediaKind media)) (mediaPath media)
