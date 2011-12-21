{-# LANGUAGE CPP, RecordWildCards #-}
module Main where

import Control.Monad.State (evalStateT)
import Clckwrks
import Clckwrks.Server
import Clckwrks.Media
import URL

#ifdef PLUGINS
import Control.Monad.State (get)
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins, withMonadIOFile)
#else
import PageMapper
#endif

import qualified Data.Text as Text
import Web.Routes.Happstack

clckwrksConfig :: ClckwrksConfig ClckURL
clckwrksConfig = ClckwrksConfig
      { clckHostname     = "localhost"
      , clckPort         = 8000 
      , clckURL          = id
      , clckJQueryPath   = "/usr/share/javascript/jquery/"
      , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
      , clckJSTreePath   = "../jstree/"
      , clckJSON2Path    = "../json2/"
      , clckThemeDir     = "../clckwrks-theme-basic/"
      , clckStaticDir    = "../static"
#ifdef PLUGINS
      , clckPageHandler  = undefined
#else
      , clckPageHandler  = staticPageHandler
#endif
      }

clckwrks :: ClckwrksConfig u -> IO ()
clckwrks cc =
  withClckwrks cc $ \clckState ->
   withMediaConfig Nothing "_uploads" $ \media ->
    simpleHTTP (nullConf { port = clckPort cc }) (route cc clckState media)

route :: ClckwrksConfig url -> ClckState -> MediaConfig -> ServerPartT IO Response
route cc clckState media =
    do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
       msum $ 
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite (clckPageHandler cc) clckState)
            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (site (clckPageHandler cc) clckState media)
            ]

routeSite :: Clck ClckURL Response -> MediaConfig -> SiteURL -> Clck SiteURL Response
routeSite pageHandler media url =
    case url of
      (C clckURL)  -> nestURL C $ routeClck pageHandler clckURL
      (M mediaURL) -> nestURL M $ runMediaT media $ routeMedia mediaURL
      
site :: Clck ClckURL Response -> ClckState -> MediaConfig -> Site SiteURL (ServerPart Response)
site ph clckState media = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m clckState) $ unRouteT (unClck $ routeSite ph media u) f

#ifdef PLUGINS
main :: IO ()
main = 
  do ph <- initPlugins 
     putStrLn "Dynamic Server Started."
     clckwrks (clckwrksConfig { clckPageHandler = dynamicPageHandler ph })

dynamicPageHandler :: PluginHandle -> Clck ClckURL Response
dynamicPageHandler ph =
  do fp <- themePath <$> get
     withMonadIOFile "PageMapper.hs" "pageMapper" ph (\pc -> pc { pcGHCArgs = [ "-i" ++ fp]  }) notLoaded page
  where
    page :: [String] -> XMLGenT (Clck url) XML -> Clck url Response
    page _errs (XMLGenT part) = toResponse <$> part
    notLoaded errs =
      internalServerError $ toResponse $ unlines errs
#else
main :: IO ()
main = 
  do putStrLn "Static Server Started."
     clckwrks clckwrksConfig

staticPageHandler :: Clck ClckURL Response
staticPageHandler = toResponse <$> unXMLGenT pageMapper
#endif


