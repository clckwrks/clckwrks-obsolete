{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, RecordWildCards, OverloadedStrings #-}
module Main where

import Clckwrks
import Clckwrks.Admin.Template (defaultAdminMenu)
import Clckwrks.Bugs
import Clckwrks.Bugs.PreProcess (bugsCmd)
import Clckwrks.Page.Atom (fixFeedConfigUUID)
import Clckwrks.Server
import Clckwrks.Media
import Clckwrks.Monad
import Clckwrks.Media.PreProcess (mediaCmd)
import Clckwrks.Page.PreProcess (pageCmd)
import Control.Monad.Reader     (ReaderT)
import Control.Monad.State (evalStateT, get, modify)
import qualified Data.ByteString.Char8 as C
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid (mappend)
import Data.Text  (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text as Text
import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)
import qualified Paths_clckwrks                 as Clckwrks
import qualified Paths_clckwrks_plugin_media    as Media
import qualified Paths_clckwrks_plugin_bugs     as Bugs
#ifdef CABAL
import qualified Paths_clckwrks_theme_clckwrks  as Theme
#endif
import System.Console.GetOpt
import System.Directory   (doesFileExist)
import System.Environment (getArgs)
import System.Exit        (exitFailure, exitSuccess)
import System.FilePath    ((</>))
import Theme.Template            (template)
import URL
import Web.Routes.Happstack
import qualified Theme.Blog as Blog

#ifdef PLUGINS
import Control.Monad.State (get)
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins, withMonadIOFile)
#else
import PageMapper
#endif

------------------------------------------------------------------------------
-- Command line options
------------------------------------------------------------------------------

-- | command-line Flags
data Flag
    = ModifyConfig (forall url. ClckwrksConfig url -> ClckwrksConfig url)
    | Help
    | Version

-- | Flag selectors
isHelp, isVersion :: Flag -> Bool
isHelp    flag = case flag of Help    -> True; _ -> False
isVersion flag = case flag of Version -> True; _ -> False

-- | Command line options.
clckwrksOpts :: ClckwrksConfig SiteURL -> [OptDescr Flag]
clckwrksOpts def =
    [ -- Option [] ["version"]       (NoArg Version)                 "Display version information"
      Option [] ["help"]          (NoArg Help)                    "Display this help message"
    , Option [] ["http-port"]     (ReqArg setPort "port")         ("Port to bind http server, default: " ++ show (clckPort def))
    , Option [] ["hostname"]      (ReqArg setHostname "hostname") ("Server hostename, default: " ++ show (clckHostname def))
    , Option [] ["base-uri"]      (ReqArg setBaseURI "URI")       ("Hostname and port, default: " ++ show ("http://" ++ clckHostname def ++ ":" ++ show (clckPort def)))
    , Option [] ["jquery-path"]   (ReqArg setJQueryPath "path")   ("path to jquery directory, default: " ++ show (clckJQueryPath def))
    , Option [] ["jqueryui-path"] (ReqArg setJQueryUIPath "path") ("path to jqueryui directory, default: " ++ show (clckJQueryUIPath def))
    , Option [] ["jstree-path"]   (ReqArg setJSTreePath "path")   ("path to jstree directory, default: " ++ show (clckJSTreePath def))
    , Option [] ["json2-path"]    (ReqArg setJSON2Path  "path")   ("path to json2 directory, default: " ++ show (clckJSON2Path def))
    , Option [] ["theme-path"]    (ReqArg setThemeDir   "path")   ("path to theme directory, default: " ++ show (clckThemeDir def))
    , Option [] ["bugs-data-path"] (ReqArg setBugsDataPath   "path")   ("path to theme directory, default: " ++ show (clckThemeDir def))
    , Option [] ["top"]           (ReqArg setTopDir     "path")   ("path to directory that holds the state directory, uploads, etc")
    , Option [] ["static"]        (ReqArg noop "ignored")         "unused"
    , Option [] ["logs"]          (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["log-mode"]      (ReqArg noop "ignored")         "unimplemented"
    , Option [] ["enable-analytics"] (NoArg setAnalytics)         "enable google analytics tracking"
    ]
    where
      noop            _   = ModifyConfig $ id
      setPort         str = ModifyConfig $ \c -> c { clckPort         = read str }
      setHostname     str = ModifyConfig $ \c -> c { clckHostname     = str      }
      setBaseURI      str = let Just (URI {uriAuthority = Just (URIAuth {uriRegName = host, uriPort = port})}) = parseAbsoluteURI str in
                            ModifyConfig $ \c -> c { clckHostname = host, clckPort = read port }
      setJQueryPath   str = ModifyConfig $ \c -> c { clckJQueryPath   = str      }
      setJQueryUIPath str = ModifyConfig $ \c -> c { clckJQueryUIPath = str      }
      setJSTreePath   str = ModifyConfig $ \c -> c { clckJSTreePath   = str      }
      setJSON2Path    str = ModifyConfig $ \c -> c { clckJSON2Path    = str      }
      setThemeDir     str = ModifyConfig $ \c -> c { clckThemeDir     = str      }
      setTopDir       str = ModifyConfig $ \c -> c { clckTopDir       = Just str }
      setAnalytics        = ModifyConfig $ \c -> c { clckEnableAnalytics = True  }
      setBugsDataPath str = ModifyConfig $ \c -> c { clckPluginDir    = Map.insert "bugs" str (clckPluginDir c) }

-- | Parse the command line arguments into a list of flags. Exits with usage
-- message, in case of failure.
parseArgs :: [OptDescr Flag] -> [String] -> IO (ClckwrksConfig url -> ClckwrksConfig url)
parseArgs opts args =
    case getOpt Permute opts args of
      (flags,_,[]) ->
          if any isHelp flags
          then do putStr (helpMessage opts)
                  exitSuccess
          else do return $ foldr (.) id [f | (ModifyConfig f) <- flags ]
      (_,_,errs)   ->
          do putStr ("Failure while parsing command line:\n"++unlines errs)
             putStr (helpMessage opts)
             exitFailure

-- | A simple usage message listing all flags possible.
helpMessage :: [OptDescr Flag] -> String
helpMessage opts =
    usageInfo header opts
    where
      header = "Usage: clckwrks [OPTION...]"


clckwrksConfig :: IO (ClckwrksConfig SiteURL)
clckwrksConfig =
    do clckDir    <- Clckwrks.getDataDir
#ifdef CABAL
       themeDir   <- Theme.getDataDir
#else
       let themeDir = "../clckwrks-theme-clckwrks/"
#endif
       mediaDir   <- Media.getDataDir
       bugsDir    <- Bugs.getDataDir
       return $ ClckwrksConfig
                  { clckHostname     = "localhost"
                  , clckPort         = 8000
                  , clckURL          = C
                  , clckJQueryPath   = "/usr/share/javascript/jquery/"
                  , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
                  , clckJSTreePath   = clckDir </> "jstree"
                  , clckJSON2Path    = clckDir </> "json2"
                  , clckThemeDir     = themeDir
                  , clckPluginDir    = Map.fromList [("media", mediaDir)
                                                    ,("bugs" , bugsDir)
                                                    ]
                  , clckStaticDir    = clckDir </> "static"
                  , clckTopDir       = Nothing
#ifdef PLUGINS
                  , clckPageHandler  = undefined
#else
                  , clckPageHandler  = staticPageHandler
#endif
                  , clckBlogHandler  = staticBlogHandler
                  , clckEnableAnalytics = False
                  }

getClckwrksConfig :: [OptDescr Flag]
                  -> ClckwrksConfig SiteURL
                  -> IO (ClckwrksConfig SiteURL)
getClckwrksConfig opts cc =
    do args <- getArgs
       f    <- parseArgs opts args
       return (f cc)

------------------------------------------------------------------------------
-- SitePlus
------------------------------------------------------------------------------

data SitePlus url a = SitePlus
    { siteSite    :: Site url a
    , siteDomain  :: Text
    , sitePort    :: Int
    , siteAppRoot :: Text
    , sitePrefix  :: Text
    , siteShowURL :: url -> [(Text, Maybe Text)] -> Text
    , siteParsePathInfo :: C.ByteString -> Either String url
    }

instance Functor (SitePlus url) where
  fmap f sitePlus = sitePlus { siteSite = fmap f (siteSite sitePlus) }

mkSitePlus :: Text
           -> Int
           -> Text
           -> Site url a
           -> SitePlus url a
mkSitePlus domain port approot site =
    SitePlus { siteSite          = site
             , siteDomain        = domain
             , sitePort          = port
             , siteAppRoot       = approot
             , sitePrefix        = prefix
             , siteShowURL       = showFn
             , siteParsePathInfo = parsePathSegments site . decodePathInfo
             }
    where
      showFn url qs =
        let (pieces, qs') = formatPathSegments site url
        in prefix `mappend` (encodePathInfo pieces (qs ++ qs'))
      prefix = Text.concat $ [ Text.pack "http://"
                             , domain
                             ] ++
                             (if port == 80 || port == 9029
                                then []
                                else [Text.pack ":", Text.pack $ show port]
                             ) ++
                             [ approot ]

runSitePlus_ :: (Happstack m) => SitePlus url (m a) -> m (Either String a)
runSitePlus_ sitePlus =
    dirs (Text.unpack (siteAppRoot sitePlus)) $
         do rq <- askRq
            let r        = runSite (sitePrefix sitePlus) (siteSite sitePlus) (map Text.pack $ rqPaths rq)
            case r of
              (Left parseError) -> return (Left parseError)
              (Right sp)   -> Right <$> (localRq (const $ rq { rqPaths = [] }) sp)
        where
          escapeSlash :: String -> String
          escapeSlash [] = []
          escapeSlash ('/':cs) = "%2F" ++ escapeSlash cs
          escapeSlash (c:cs)   = c : escapeSlash cs

runSitePlus :: (Happstack m) => SitePlus url (m a) -> m a
runSitePlus sitePlus =
    do r <- runSitePlus_ sitePlus
       case r of
         (Left _)  -> mzero
         (Right a) -> return a

initPlugins :: ClckT SiteURL IO ()
initPlugins =
    do showFn <- askRouteFn
       let mediaCmd' :: forall url m. (Monad m) => (Text -> ClckT url m Builder)
           mediaCmd' = mediaCmd (\u p -> showFn (M u) p)
       addPreProcessor "media" mediaCmd'

       let bugsCmd' :: forall url m. (Functor m, Monad m) => (Text -> ClckT url m Builder)
           bugsCmd' = bugsCmd (\u p -> showFn (B u) p)
       addPreProcessor "bugs" bugsCmd'

       let pageCmd' :: forall url m. (Functor m, MonadIO m) => (Text -> ClckT url m Builder)
           pageCmd' = pageCmd (\u p -> showFn (C u) p)
       addPreProcessor "page" pageCmd'

       nestURL M $ addMediaAdminMenu
       dm <- nestURL C $ defaultAdminMenu
       mapM_ addAdminMenu dm

clckwrks :: ClckwrksConfig SiteURL -> IO ()
clckwrks cc =
    do checkResources cc
       withClckwrks cc $ \clckState ->
           withMediaConfig Nothing "_media_uploads" $ \mediaConf ->
           withBugsConfig  Nothing "_bugs_attachments" $ \bugsConf ->
               let -- site     = mkSite (clckPageHandler cc) clckState mediaConf
                   site     = mkSite2 cc mediaConf bugsConf
                   sitePlus = mkSitePlus (Text.pack $ clckHostname cc) (clckPort cc) Text.empty site
               in
                 do clckState'    <- execClckT (siteShowURL sitePlus) clckState $ initPlugins
                    let sitePlus' = fmap (evalClckT (siteShowURL sitePlus) clckState') sitePlus
                    putStrLn $ "Listening on port " ++ show (clckPort cc)
                    simpleHTTP (nullConf { port = clckPort cc }) (route cc sitePlus')

route :: Happstack m => ClckwrksConfig SiteURL -> SitePlus SiteURL (m Response) -> m Response
route cc sitePlus =
    do decodeBody (defaultBodyPolicy "/tmp/" (10 * 10^6)  (1 * 10^6)  (1 * 10^6))
       msum $
            [ jsHandlers cc
            , dir "favicon.ico" $ notFound (toResponse ())
            , dir "static"      $ serveDirectory DisableBrowsing [] (clckStaticDir cc)
            , dir "login"       $ seeOther ((siteShowURL sitePlus) (C $ Auth $ AuthURL A_Login) []) (toResponse ())
            , dir "admin"       $ seeOther ((siteShowURL sitePlus) (C $ Admin Console) []) (toResponse ())
            , dir "blog" $ dir "atom.xml" $ seeOther ((siteShowURL sitePlus) (C $ AtomFeed) []) (toResponse ())
            , dir "blog"        $ seeOther ((siteShowURL sitePlus) (C $ Blog) []) (toResponse ())
            , runSitePlus sitePlus
            ]

{-

we can't register the pp callbacks instead the nestURL because then
the only callbacks will only be available when that route is active.

it is 'tricky' to register the callbacks outside, because the
callbacks might require information that is only available 'inside'
the monad. But, of course, that is silly now that we think about
it. Because that monad is only available when processing the route. But when doing the pp, that route may not be the one we are doing.

So, the reason it is hard to get the monad into the callback is because we shouldn't. The pp has to assume that the route being processed is not one of those.

Well, that is not actually a problem. The monad is really an environment in which a computation can run. And we can create that environment multiple ways.

The issue with the MediaT monad is that it includes the MediaURL. And so to work with that, we need to specify how to turn a MediaURL into a SiteURL.

That is something we normally do in routeSite via 'nestURL M'. But that means we have to repeat ourselves.

we could have a function like withMediaT to contruct a temporary MediaT monad to be used when registering the callback. Though there is a danger there, because some of the information use to register the callback might become stale.

In theory, we would like to do some stuff in the ClckT monad before start listening to incoming requests. However, to run the ClckT monad we need to provide the show function. Normally that is done transparently via implSite / site / etc.

Though it seems the information we need comes from Site not implSite.
-}
routeSite :: ClckwrksConfig u -> MediaConfig -> BugsConfig -> SiteURL -> Clck SiteURL Response
routeSite cc mediaConfig bugsConfig url =
    do case url of
        (C clckURL)  -> nestURL C $ routeClck cc clckURL
        (M mediaURL) ->
            do showFn <- askRouteFn
               -- FIXME: it is a bit silly that we wait this  long to set the mediaClckURL
               -- would be better to do it before we forkIO on simpleHTTP
               nestURL M $ runMediaT (mediaConfig { mediaClckURL = (showFn . C) })  $ routeMedia mediaURL
        (B bugsURL) ->
            do showFn <- askRouteFn
               let deRoute :: (ClckURL -> [(Text, Maybe Text)] -> Text) -> Clck ClckURL a -> Clck url a
                   deRoute sf (ClckT (RouteT r)) = (ClckT (RouteT (\nsf -> (r sf))))

               let template' :: ( EmbedAsChild BugsM headers
                                , EmbedAsChild BugsM body
                                ) => String
                             -> headers
                             -> body
                             -> XMLGenT BugsM XML
                   template' ttl hdrs bdy =
                       do hdrXml <- map unClckChild <$> asChild hdrs
                          bdyXml <- map unClckChild <$> asChild bdy
                          mapXMLGenT (mapClckT lift . (deRoute (showFn . C))) $ template ttl hdrXml bdyXml

               nestURL B $ runBugsT (bugsConfig { bugsClckURL      = (showFn . C)
                                                , bugsPageTemplate = template'
                                                }) $ routeBugs bugsURL

-- FIXME: something seems weird here.. we do not use the 'f' in route'
mkSite2 :: ClckwrksConfig u -> MediaConfig -> BugsConfig -> Site SiteURL (ClckT SiteURL (ServerPartT IO) Response)
mkSite2 cc mediaConfig bugsConfig = setDefault (C $ ViewPage $ PageId 1) $ mkSitePI route'
    where
      route' :: (SiteURL -> [(Text.Text, Maybe Text.Text)] -> Text.Text) -> SiteURL -> ClckT SiteURL (ServerPartT IO) Response
      route' f url =
          routeSite cc mediaConfig bugsConfig url


#ifdef PLUGINS
main :: IO ()
main =
  do ph <- initPlugins
     putStrLn "Dynamic Server Started."
     defCC <- clckwrksConfig
     cc    <- getClckwrksConfig (clckwrksOpts defCC) defCC
     clckwrks (cc { clckPageHandler = dynamicPageHandler ph })

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
     defCC <- clckwrksConfig
     cc    <- getClckwrksConfig (clckwrksOpts defCC) defCC
     clckwrks cc

staticPageHandler :: Clck ClckURL Response
staticPageHandler = toResponse <$> unXMLGenT pageMapper
#endif

staticBlogHandler :: Clck ClckURL Response
staticBlogHandler = toResponse <$> unXMLGenT Blog.page

------------------------------------------------------------------------------
-- javascript check
------------------------------------------------------------------------------

checkResources :: ClckwrksConfig url -> IO ()
checkResources cc =
    do let jquery = (clckJQueryPath cc) </> "jquery.js"
       checkResource jquery $ unlines [ "Could not find: " ++ jquery
                                      , "Please make sure jquery is installed. Use --jquery-path to set the path."
                                      , "Download from http://jquery.com/"
                                      ]
       let json2 = (clckJSON2Path cc) </> "json2.js"
       checkResource json2  $ unlines [ "Could not find: " ++ json2
                                      , "Please make sure json2.js is installed. Use --json2-path to set the path."
                                      , "Download from https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
                                      ]

       let jstree = (clckJSTreePath cc) </> "jquery.jstree.js"
       checkResource jstree $ unlines [ "Could not find: " ++ jstree
                                      , "Please make sure jstree is installed. Use --jstree-path to set the path."
                                      , "Download from http://github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                      ]

checkResource  :: FilePath -> String -> IO ()
checkResource fp msg =
    do e <- doesFileExist fp
       when (not e) $ putStrLn msg
