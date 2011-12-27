{-# LANGUAGE CPP, RecordWildCards #-}
module Main where

import Control.Monad.State (evalStateT)
import Clckwrks
import Clckwrks.Server
import Clckwrks.Media
import Clckwrks.Media.PreProcess (mediaCmd)
import qualified Data.ByteString.Char8 as C
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid (mappend)
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

clckwrks' :: ToMessage a =>
             Text.Text
          -> ClckwrksConfig url
          -> (ClckState -> MediaConfig -> RouteT SiteURL (ServerPartT IO) a)
          -> IO ()
clckwrks' approot cc handler =
  withClckwrks cc $ \clckState ->
   withMediaConfig Nothing "_uploads" clckState $ \clckState' mediaConfig ->
       let s = site (clckPageHandler cc) clckState' mediaConfig
       in unRouteT (r clckState mediaConfig) (showUrlFn s)
    where
      showUrlFn s url qs =
        let (pieces, qs') = formatPathSegments s url
        in approot `mappend` encodePathInfo pieces (qs ++ qs')
      r :: ClckState -> MediaConfig -> RouteT SiteURL IO ()
      r clckState mediaConfig = 
          do rf <- askRouteFn
             let clckState' = clckState { preProcessorCmds = Map.insert (Text.pack "media") (mediaCmd (\u p -> rf (M u) p)) (preProcessorCmds clckState) }
             clckwrks_ cc (handler clckState' mediaConfig)

clckwrks_ :: (ToMessage a) => ClckwrksConfig u -> RouteT SiteURL (ServerPartT IO) a -> RouteT SiteURL IO ()
clckwrks_ cc (RouteT handler) = RouteT $ \showURLFn ->
    simpleHTTP (nullConf { port = clckPort cc }) (handler showURLFn)


clckwrks cc = clckwrks' Text.empty cc (route' Text.empty cc)

route' :: Text.Text -> ClckwrksConfig url -> ClckState -> MediaConfig -> RouteT SiteURL (ServerPartT IO) Response
route' approot cc clckState mediaConfig =
    (lift $ do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
               msum $  [ jsHandlers cc
                      , dir "favicon.ico" $ notFound (toResponse ())
                      , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite (clckPageHandler cc) clckState)
--            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (site (clckPageHandler cc) clckState media)
                      ])
     `mplus` r
    where
      r :: RouteT SiteURL (ServerPartT IO) Response
      r = dirs (Text.unpack approot) $
            do rq <- askRq
               let pathInfo = intercalate "/" (map escapeSlash (rqPaths rq))
                   s = site (clckPageHandler cc) clckState mediaConfig
               case parsePathSegments s $ decodePathInfo (C.pack pathInfo) of
                 (Left parseError) -> notFound $ toResponse parseError
                 (Right url) ->
                     mapRouteT (mapServerPartT (\m -> evalStateT m clckState)) $ unClck $ routeSite (clckPageHandler cc) mediaConfig url

{-
                   f        = runSite (domain `Text.append` approot) siteSpec (C.pack pathInfo)
               case f of
                 (Left parseError) -> return (Left parseError)
                 (Right sp)   -> Right <$> (localRq (const $ rq { rqPaths = [] }) sp)
-}
      escapeSlash :: String -> String
      escapeSlash [] = []
      escapeSlash ('/':cs) = "%2F" ++ escapeSlash cs
      escapeSlash (c:cs)   = c : escapeSlash cs
    
clckwrksOld :: ClckwrksConfig u -> IO ()
clckwrksOld cc =
  withClckwrks cc $ \clckState ->
   withMediaConfig Nothing "_uploads" clckState $ \clckState' media ->
    simpleHTTP (nullConf { port = clckPort cc }) (route cc clckState' media)

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
    do 
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


