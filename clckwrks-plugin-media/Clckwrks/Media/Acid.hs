{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Media.Acid where

import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.IxSet             (IxSet, (@=), getOne, empty, toList, updateIx)
import Data.SafeCopy          (base, deriveSafeCopy)
import Clckwrks.Media.Types   (Medium(..), MediumId(..))

data MediaState = MediaState 
    { nextMediumId :: MediumId
    , media        :: IxSet Medium
    }
$(deriveSafeCopy 0 'base ''MediaState)

initialMediaState :: MediaState
initialMediaState = MediaState
    { nextMediumId = MediumId 0
    , media        = empty
    }

-- | get the next unused 'MediaId'
genMediumId :: Update MediaState MediumId
genMediumId =
    do ms@MediaState{..} <- get
       put $ ms { nextMediumId = MediumId $ succ $ unMediumId $ nextMediumId }
       return nextMediumId

-- | get 'Media' by 'MediumId'
getMediumById :: MediumId -> Query MediaState (Maybe Medium)
getMediumById mid =
    do MediaState{..} <- ask
       return $ getOne (media @= mid)

-- | store 'Media' in the state. Will overwrite an existing entry with the same 'MediumId'
putMedium :: Medium -> Update MediaState ()
putMedium m =
    do ms@MediaState{..} <- get
       put $ ms { media = updateIx (mediumId m) m media }

allMediumIds :: Query MediaState [MediumId]
allMediumIds =
    do MediaState{..} <- ask
       return $ map mediumId (toList media)

$(makeAcidic ''MediaState 
   [ 'genMediumId
   , 'getMediumById
   , 'putMedium
   , 'allMediumIds
   ]
 )
