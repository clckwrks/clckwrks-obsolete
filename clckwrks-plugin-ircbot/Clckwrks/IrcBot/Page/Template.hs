{-# LANGUAGE FlexibleContexts #-}
module Clckwrks.IrcBot.Page.Template where

import Clckwrks
import Clckwrks.IrcBot.Monad
import Control.Monad.Reader
import HSP
import Happstack.Server.HSP.HTML

template :: ( EmbedAsChild (Clck ClckURL) headers
            , EmbedAsChild (Clck ClckURL) body
            ) =>
            String
         -> headers
         -> body
         -> IrcBotM Response
template ttl hdrs bdy =
    do pageTemplate <- ircBotPageTemplate <$> ask
       fmap toResponse $ mapClckT lift $ unXMLGenT $
            pageTemplate ttl hdrs bdy

