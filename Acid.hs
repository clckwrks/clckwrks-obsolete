module Acid where

import Control.Exception           (bracket)
import Data.Acid                   (AcidState, openAcidStateFrom, createCheckpointAndClose)
import Data.Maybe                  (fromMaybe)
import Page.Acid                   (PageState       , initialPageState)
import Happstack.Auth.Core.Auth    (AuthState       , initialAuthState)
import Happstack.Auth.Core.Profile (ProfileState    , initialProfileState)
import System.FilePath             ((</>))

data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    , acidPage        :: AcidState PageState
    }

class GetAcidState st where
    getAcidState :: Acid -> AcidState st

instance GetAcidState AuthState where
    getAcidState = acidAuth

instance GetAcidState ProfileState where
    getAcidState = acidProfile

instance GetAcidState PageState where
    getAcidState = acidPage
    

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openAcidStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openAcidStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
    bracket (openAcidStateFrom (basePath </> "page")        initialPageState)        (createCheckpointAndClose) $ \page ->
        f (Acid auth profile page)
