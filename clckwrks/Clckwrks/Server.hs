{-# LANGUAGE RecordWildCards #-}
module Clckwrks.Server where

import Clckwrks
import Clckwrks.BasicTemplate      (basicTemplate)
import Clckwrks.Admin.Route        (routeAdmin)
import Clckwrks.Page.Atom          (handleAtomFeed)
import Clckwrks.Page.PreProcess    (pageCmd)
import Clckwrks.ProfileData.Route  (routeProfileData)
import Clckwrks.ProfileData.Types  (Role(..))
import Clckwrks.ProfileData.URL    (ProfileDataURL(..))
import Control.Concurrent.STM      (atomically, newTVar)
import Control.Monad.State         (get, evalStateT)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import Data.String                 (fromString)
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.UUID         as UUID
import Happstack.Auth              (handleAuthProfile)
import Happstack.Server.FileServe.BuildingBlocks (guessContentTypeM, isSafePath, serveFile)
import Network.URI                 (unEscapeString)
import System.FilePath             ((</>), makeRelative, splitDirectories)
import Web.Routes.Happstack        (implSite)

data ClckwrksConfig url = ClckwrksConfig
    { clckHostname        :: String
    , clckPort            :: Int
    , clckURL             :: ClckURL -> url
    , clckJQueryPath      :: FilePath
    , clckJQueryUIPath    :: FilePath
    , clckJSTreePath      :: FilePath
    , clckJSON2Path       :: FilePath
    , clckThemeDir        :: FilePath
    , clckPluginDir       :: Map Text FilePath
    , clckStaticDir       :: FilePath
    , clckPageHandler     :: Clck ClckURL Response
    , clckBlogHandler     :: Clck ClckURL Response
    , clckTopDir          :: Maybe FilePath
    , clckEnableAnalytics :: Bool
    }

withClckwrks :: ClckwrksConfig url -> (ClckState -> IO b) -> IO b
withClckwrks cc action =
    do withAcid (fmap (\top -> top </> "_state") (clckTopDir cc)) $ \acid ->
           do u <- atomically $ newTVar 0
              let clckState = ClckState { acidState        = acid
                                        , currentPage      = PageId 0
                                        , themePath        = clckThemeDir cc
                                        , pluginPath       = clckPluginDir cc
                                        , componentPrefix  = Prefix (fromString "clckwrks")
                                        , uniqueId         = u
                                        , preProcessorCmds = Map.empty
                                        , adminMenus       = []
                                        , enableAnalytics  = clckEnableAnalytics cc
                                        }
              action clckState

simpleClckwrks :: ClckwrksConfig u -> IO ()
simpleClckwrks cc =
  withClckwrks cc $ \clckState ->
    simpleHTTP (nullConf { port = clckPort cc }) (handlers cc clckState)
  where
    handlers cc clckState =
       do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
          msum $
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
            , implSite (Text.pack $ "http://" ++ clckHostname cc ++ ":" ++ show (clckPort cc)) (Text.pack "") (clckSite cc clckState)
            ]

jsHandlers :: (Happstack m) => ClckwrksConfig u -> m Response
jsHandlers c =
  msum [ dir "jquery"      $ serveDirectory DisableBrowsing [] (clckJQueryPath c)
       , dir "jquery-ui"   $ serveDirectory DisableBrowsing [] (clckJQueryUIPath c)
       , dir "jstree"      $ serveDirectory DisableBrowsing [] (clckJSTreePath c)
       , dir "json2"       $ serveDirectory DisableBrowsing [] (clckJSON2Path c)
       ]

checkAuth :: (Happstack m, Monad m) => ClckURL -> ClckT ClckURL m ClckURL
checkAuth url =
    case url of
      ViewPage{}           -> return url
      Blog{}               -> return url
      AtomFeed{}           -> return url
      ThemeData{}          -> return url
      PluginData{}         -> return url
      Admin{}              -> requiresRole (Set.singleton Administrator) url
      Auth{}               -> return url
      Profile EditProfileData{}    -> requiresRole (Set.fromList [Administrator, Visitor]) url
      Profile EditProfileDataFor{} -> requiresRole (Set.fromList [Administrator]) url
      Profile CreateNewProfileData -> return url

routeClck :: ClckwrksConfig u -> ClckURL -> Clck ClckURL Response
routeClck cc url' =
    do url <- checkAuth url'
       setUnique 0
       case url of
         (ViewPage pid) ->
           do setCurrentPage pid
              (clckPageHandler cc)
         (Blog) ->
           do clckBlogHandler cc
         AtomFeed ->
             do handleAtomFeed
         (ThemeData fp')  ->
             do fp <- themePath <$> get
                let fp'' = makeRelative "/" (unEscapeString fp')
                if not (isSafePath (splitDirectories fp''))
                   then notFound (toResponse ())
                   else serveFile (guessContentTypeM mimeTypes) (fp </> "data" </> fp'')
         (PluginData plugin fp')  ->
             do ppm <- pluginPath <$> get
                case Map.lookup plugin ppm of
                  Nothing -> notFound (toResponse ())
                  (Just pp) ->
                      do let fp'' = makeRelative "/" (unEscapeString fp')
                         if not (isSafePath (splitDirectories fp''))
                           then notFound (toResponse ())
                           else serveFile (guessContentTypeM mimeTypes) (pp </> "data" </> fp'')
         (Admin adminURL) ->
             routeAdmin adminURL
         (Profile profileDataURL) ->
             do nestURL Profile $ routeProfileData profileDataURL
         (Auth apURL) ->
             do Acid{..} <- acidState <$> get
                u <- showURL $ Profile CreateNewProfileData
                nestURL Auth $ handleAuthProfile acidAuth acidProfile basicTemplate Nothing Nothing u apURL

routeClck' :: ClckwrksConfig u -> ClckState -> ClckURL -> RouteT ClckURL (ServerPartT IO) Response
routeClck' cc clckState url =
    mapRouteT (\m -> evalStateT m clckState) $ (unClckT $ routeClck cc url)

clckSite :: ClckwrksConfig u -> ClckState -> Site ClckURL (ServerPart Response)
clckSite cc clckState = setDefault (ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' f u = unRouteT (routeClck' cc clckState u) f