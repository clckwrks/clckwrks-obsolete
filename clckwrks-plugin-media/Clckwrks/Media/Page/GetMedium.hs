module Clckwrks.Media.Page.GetMedium where

import Control.Applicative  ((<$>))
import Control.Monad.Reader (ask)
import Clckwrks             (query)
import Clckwrks.Media.Acid  (GetMediumById(..))
import Clckwrks.Media.Types (MediumId(..), Medium(..), mediumContentType)
import Clckwrks.Media.Monad (MediaM, MediaConfig(..))
import Happstack.Server     (Response, asContentType, notFound, serveFile, toResponse)
import System.FilePath      ((</>))

getMedium :: MediumId -> MediaM Response
getMedium mid =
    do mMedium <- query (GetMediumById mid)
       case mMedium of
         Nothing -> do notFound $ toResponse $ "Invalid medium id " ++ show (unMediumId mid)
         (Just medium) ->
             do md <- mediaDirectory <$> ask
                serveFile (asContentType (mediumContentType $ mediumKind medium)) (md </> (mediumPath medium))

