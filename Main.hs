module Main where

import Admin.Route         (routeAdmin)
import Control.Monad.State (evalStateT)
import CMS
import qualified Data.Text as Text
import Happstack.Plugins.Plugins (PluginHandle, func, initPlugins)
import ProfileData.Route    (routeProfileData)
import Web.Routes.Happstack (implSite)

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
    do decodeBody (defaultBodyPolicy "/tmp/" (1*10^6) (1*10^6) (10*10^6))
       msum 
        [ dir "favicon.ico" $ notFound (toResponse ())
        , dir "static"      $ serveDirectory DisableBrowsing [] "static"
        , implSite (Text.pack "http://localhost:8000") (Text.pack "") (cms ph cmsState)
        ]

route :: PluginHandle -> SiteURL -> CMS SiteURL Response
route ph url =
    case url of
      (ViewPage pid) -> 
          do setCurrentPage pid
             withSymbol ph "Page.hs" "page" page
      (Admin adminURL) ->
          routeAdmin adminURL
      (Profile profileDataURL) ->
          nestURL Profile $ routeProfileData profileDataURL

cms :: PluginHandle -> CMSState -> Site SiteURL (ServerPart Response)
cms ph cmsState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m cmsState) $ unRouteT (unCMS $ route ph u) f

withSymbol :: (MonadIO m) => PluginHandle -> FilePath -> String -> (a -> m b) -> m b
withSymbol ph fp sym f =
    do r <- liftIO $ func ph fp sym
       case r of
         (Left e)  -> error (unlines e)
         (Right a) -> f a

page :: XMLGenT (CMS url) XML -> CMS url Response
page (XMLGenT part) = toResponse <$> part
