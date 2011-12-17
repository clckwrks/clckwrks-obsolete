{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Media.Acid where

import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.IxSet             (IxSet, (@=), getOne, updateIx)
import Data.SafeCopy          (base, deriveSafeCopy)
import Clckwrks.Media.Types   (Media(..), MediaId(..))

data MediaState = MediaState 
    { nextMediaId :: MediaId
    , media       :: IxSet Media
    }
$(deriveSafeCopy 0 'base ''MediaState)

-- | get the next unused 'MediaId'
genMediaId :: Update MediaState MediaId
genMediaId =
    do ms@MediaState{..} <- get
       put $ ms { nextMediaId = MediaId $ succ $ unMediaId $ nextMediaId }
       return nextMediaId

-- | get 'Media' by 'MediaId'
getMediaById :: MediaId -> Query MediaState (Maybe Media)
getMediaById mid =
    do MediaState{..} <- ask
       return $ getOne (media @= mid)

-- | store 'Media' in the state. Will overwrite an existing entry with the same 'MediaId'
putMedia :: Media -> Update MediaState ()
putMedia m =
    do ms@MediaState{..} <- get
       put $ ms { media = updateIx (mediaId m) m media }

$(makeAcidic ''MediaState 
   [ 'genMediaId
   , 'getMediaById
   , 'putMedia
   ]
 )
