module Clckwrks.Media.Page.Preview where

import Control.Monad.Reader
import Control.Monad.Trans
import Clckwrks
import Clckwrks.IOThread
import Clckwrks.Media.Acid
import Clckwrks.Media.Monad
import Clckwrks.Media.Types
import Happstack.Server

previewMedium :: MediumId -> MediaM Response
previewMedium mid =
    do size <- lookSize <|> pure Tall
       mFp <- previewMediumFilePath mid size
       case mFp of 
         Nothing -> notFound $ toResponse $ "Invalid MediumId " ++ show (unMediumId mid)
         (Just fp) -> serveFile (guessContentTypeM mimeTypes) fp

lookSize :: MediaM PreviewSize
lookSize =
    do txt <- look "size"
       case txt of
         "tall"   -> return Tall
         "grande" -> return Grande
         "venti"  -> return Venti
         _        -> mzero

previewMediumFilePath :: MediumId -> PreviewSize -> MediaM (Maybe FilePath)
previewMediumFilePath mediumId size =
    do filterChan <- mediaIOThread <$> ask
       res <- query (GetMediumById mediumId)
       case res of
         Nothing -> return Nothing
         (Just medium) -> Just <$> (liftIO (ioRequest filterChan (medium, size)))
