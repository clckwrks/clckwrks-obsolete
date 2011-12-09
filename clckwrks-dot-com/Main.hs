{-# LANGUAGE CPP, RecordWildCards #-}
module Main where

import Clckwrks
import ClckwrksServer
import PageMapper

#ifdef PLUGINS
import Control.Monad.State (get)
import System.Plugins.Auto (PluginHandle, PluginConf(..), defaultPluginConf, initPlugins)
import System.Plugins.Auto.Reloader (func)
#endif

data SiteURL = C ClckURL

clckwrksConfig :: ClckwrksConfig SiteURL
clckwrksConfig = ClckwrksConfig
      { clckHostname     = "localhost"
      , clckPort         = 8000 
      , clckURL          = C
      , clckJQueryPath   = "/usr/share/javascript/jquery/"
      , clckJQueryUIPath = "/usr/share/javascript/jquery-ui/"
      , clckJSTreePath   = "../jstree/"
      , clckJSON2Path    = "../json2/"
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
#else
main :: IO ()
main = 
  do putStrLn "Static Server Started."
     simpleClckwrks clckwrksConfig
#endif

#ifdef PLUGINS
dynamicPageHandler :: PluginHandle -> Clck ClckURL Response
dynamicPageHandler ph =
  do fp <- themePath <$> get
     let pc = (defaultPluginConf { pcGHCArgs = [ "-i" ++ fp]  })
     withSymbol ph "PageMapper.hs" "pageMapper" pc page
  where
    page :: XMLGenT (Clck url) XML -> Clck url Response
    page (XMLGenT part) = toResponse <$> part

withSymbol :: (MonadIO m) => PluginHandle -> FilePath -> String -> PluginConf -> (a -> m b) -> m b
withSymbol ph fp sym pc f =
    do (errs, r) <- liftIO $ func ph fp sym pc
       case r of
         Nothing  -> error (unlines errs)
         (Just a) -> f a
#else
staticPageHandler :: Clck ClckURL Response
staticPageHandler = toResponse <$> unXMLGenT pageMapper
#endif


