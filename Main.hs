module Main where

import Acid
import Admin.Route (routeAdmin)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import CMS
import qualified Data.Text as Text
import Happstack.Server
import Happstack.Plugins.Plugins
import HSP
import HSP.ServerPartT
import Happstack.Server.HSP.HTML
import Web.Routes
import Web.Routes.Happstack

main :: IO ()
main = 
    do ph <- initPlugins
       withAcid Nothing $ \acid ->
           do let cmsState = CMSState { acidState = acid 
                                      , currentPage = PageId 0
                                      }
              putStrLn "starting..."
              simpleHTTP nullConf (handlers ph cmsState)

handlers :: PluginHandle -> CMSState -> ServerPart Response
handlers ph cmsState =
    msum 
     [ dir "favicon.ico" $ notFound (toResponse ())
     , implSite (Text.pack "http://localhost:8000") (Text.pack "") (cms ph cmsState)
     ]

route :: PluginHandle -> CMSURL -> CMS Response
route ph url =
    case url of
      (Page pid) -> 
          do setCurrentPage pid
             withSymbol ph "Page.hs" "page" page
      (Admin adminURL) ->
          routeAdmin adminURL

cms :: PluginHandle -> CMSState -> Site CMSURL (ServerPart Response)
cms ph cmsState = setDefault (Page $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m cmsState) $ unRouteT (unCMS $ route ph u) f

withSymbol :: (MonadIO m) => PluginHandle -> FilePath -> String -> (a -> m b) -> m b
withSymbol ph fp sym f =
    do r <- liftIO $ func ph fp sym
       case r of
         (Left e)  -> error (unlines e)
         (Right a) -> f a

page :: XMLGenT CMS XML -> CMS Response
page (XMLGenT part) = toResponse <$> part
