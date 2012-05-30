{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.Template where

import Clckwrks
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.URL
import Control.Monad.Reader
import HSP
import Happstack.Server.HSP.HTML

template :: ( EmbedAsChild BugsM headers
            , EmbedAsChild BugsM body
            ) =>
            String
         -> headers
         -> body
         -> BugsM Response
template ttl hdrs bdy =
    do pageTemplate <- bugsPageTemplate <$> ask
       fmap toResponse $ unXMLGenT $
            pageTemplate ttl <%> <link rel="stylesheet" type="text/css" href=(BugsData "style.css") /> <% hdrs %></%> bdy
