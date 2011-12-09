{-# LANGUAGE CPP, RecordWildCards #-}
module Main where

import Clckwrks
import ClckwrksServer

#ifdef PLUGINS
import Control.Monad.State (get)
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins, withMonadIOFile)
#else
import PageMapper
#endif

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
      
#ifdef PLUGINS
main :: IO ()
main = 
  do ph <- initPlugins 
     putStrLn "Dynamic Server Started."
     simpleClckwrks (clckwrksConfig { clckPageHandler = dynamicPageHandler ph })

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
     simpleClckwrks clckwrksConfig

staticPageHandler :: Clck ClckURL Response
staticPageHandler = toResponse <$> unXMLGenT pageMapper
#endif


