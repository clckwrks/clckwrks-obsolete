{-# LANGUAGE FlexibleContexts #-}
module Clckwrks.Bugs.Page.Template where

import Clckwrks
import Clckwrks.Bugs.Monad
import Control.Monad.Reader
import HSP
import Happstack.Server.HSP.HTML

template :: ( EmbedAsChild (Clck ClckURL) headers
            , EmbedAsChild (Clck ClckURL) body
            ) =>
            String
         -> headers
         -> body
         -> BugsM Response
template ttl hdrs bdy =
    do pageTemplate <- bugsPageTemplate <$> ask
       fmap toResponse $ mapClckT lift $ unXMLGenT $
            pageTemplate ttl hdrs bdy
