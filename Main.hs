{-# LANGUAGE RecordWildCards #-}
module Main where

import Admin.Route          (routeAdmin)
import Admin.Template       (template)
import Control.Monad.State  (get, evalStateT)
import CMS
import qualified Data.Text as Text
import Data.String          (fromString)
import Happstack.Auth
import Happstack.Plugins.Plugins (PluginHandle, func, initPlugins)
import ProfileData.Route    (routeProfileData)
import ProfileData.URL      (ProfileDataURL(..))
import Web.Routes.Happstack (implSite)

data ClckwrksConfig url = ClckwrksConfig
    { clckHostname     :: String
    , clckPort         :: Int
    , clckURL          :: ClckURL -> url
    , clckJQueryPath   :: FilePath
    , clckJQueryUIPath :: FilePath
    , clckJSTreePath   :: FilePath
    , clckJSON2Path    :: FilePath
    }
    
defaultClckwrksConfig :: ClckwrksConfig ClckURL
defaultClckwrksConfig = ClckwrksConfig
      { clckHostname     = "localhost"
      , clckPort         = 8000 
      , clckURL          = id
      , clckJQueryPath   = "/usr/share/javascript/jquery/"
      , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
      , clckJSTreePath   = "jstree/"
      , clckJSON2Path    = "json2/"
      }
    
data SiteURL = Clck ClckURL

main :: IO ()
main = 
  let c = defaultClckwrksConfig  { clckURL = Clck }
  in simpleClckwrks c
        
withClckwrks ::  (PluginHandle -> CMSState -> IO b) -> IO b
withClckwrks action =
    do ph <- initPlugins
       withAcid Nothing $ \acid ->
           do let clckState = CMSState { acidState       = acid 
                                       , currentPage     = PageId 0
                                       , componentPrefix = Prefix (fromString "clckwrks")
                                       , uniqueId        = 0
                                       }
              action ph clckState
  
simpleClckwrks :: ClckwrksConfig u -> IO ()
simpleClckwrks cc =
  withClckwrks $ \ph clckState ->
    simpleHTTP (nullConf { port = clckPort cc }) (handlers ph clckState)
  where
    handlers ph clckState =
       msum $ 
         [ jsHandlers cc
         , dir "favicon.ico" $ notFound (toResponse ())
         , dir "static"      $ serveDirectory DisableBrowsing [] "static"
         , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (cmsSite ph clckState)
         ]
              
jsHandlers :: ClckwrksConfig u -> ServerPart Response
jsHandlers c =
  msum [ dir "jquery"      $ serveDirectory DisableBrowsing [] (clckJQueryPath c)
       , dir "jquery-ui"   $ serveDirectory DisableBrowsing [] (clckJQueryUIPath c)
       , dir "jstree"      $ serveDirectory DisableBrowsing [] (clckJSTreePath c)
       , dir "json2"       $ serveDirectory DisableBrowsing [] (clckJSON2Path c)
       ]
{-
handlers :: PluginHandle -> CMSState -> ServerPart Response
handlers ph cmsState =
    do decodeBody (defaultBodyPolicy "/tmp/" (1*10^6) (1*10^6) (10*10^6))
       msum 
        [ dir "favicon.ico" $ notFound (toResponse ())
        , dir "static"      $ serveDirectory DisableBrowsing [] "static"
        , dir "jquery"      $ serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery/"
        , dir "jquery-ui"   $ serveDirectory DisableBrowsing [] "/usr/share/javascript/jquery-ui/"
        , dir "jstree"      $ serveDirectory DisableBrowsing [] "jstree/"
        , dir "json2"       $ serveDirectory DisableBrowsing [] "json2/"
        , implSite (Text.pack "http://192.168.0.8:8000") (Text.pack "") (cmsSite ph cmsState)
        ]
-}
routeClck :: PluginHandle -> ClckURL -> CMS ClckURL Response
routeClck ph url =
    do setUnique 0
       case url of
         (ViewPage pid) ->
             do setCurrentPage pid
                withSymbol ph "PageMapper.hs" "pageMapper" page
         (Admin adminURL) ->
             routeAdmin adminURL
         (Profile profileDataURL) ->
             nestURL Profile $ routeProfileData profileDataURL
         (Auth apURL) ->
             do Acid{..} <- acidState <$> get
                u <- showURL $ Profile CreateNewProfileData
                nestURL Auth $ handleAuthProfile acidAuth acidProfile template Nothing Nothing u apURL

cmsSite :: PluginHandle -> CMSState -> Site ClckURL (ServerPart Response)
cmsSite ph cmsState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u =
          mapServerPartT (\m -> evalStateT m cmsState) $ unRouteT (unCMS $ routeClck ph u) f

withSymbol :: (MonadIO m) => PluginHandle -> FilePath -> String -> (a -> m b) -> m b
withSymbol ph fp sym f =
    do r <- liftIO $ func ph fp sym
       case r of
         (Left e)  -> error (unlines e)
         (Right a) -> f a

page :: XMLGenT (CMS url) XML -> CMS url Response
page (XMLGenT part) = toResponse <$> part
