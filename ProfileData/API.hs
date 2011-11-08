{-# LANGUAGE RecordWildCards #-}
module ProfileData.API
    ( getProfileData
    , whoami
    )  where

import Acid(Acid(..))
import Control.Applicative ((<$>))
import Control.Monad.State (get)
import CMSMonad
import ProfileData.Acid
import ProfileData.Types
import Happstack.Auth

getProfileData :: UserId -> CMS url (Maybe ProfileData)
getProfileData uid = query (GetProfileData uid)

whoami :: CMS url (Maybe UserId)
whoami =
    do Acid{..} <- acidState <$> get
       getUserId acidAuth acidProfile

