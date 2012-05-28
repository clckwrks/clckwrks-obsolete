{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Bugs.Acid where

import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.IxSet             (IxSet, (@=), getOne, empty, toList, updateIx)
import Data.SafeCopy          (base, deriveSafeCopy)
import Clckwrks.Bugs.Types    (Bug(..), BugId(..))

-- | 'BugsState' stores all the bugs
data BugsState = BugsState
    { nextBugId :: BugId
    , bugs        :: IxSet Bug
    }
$(deriveSafeCopy 0 'base ''BugsState)

-- | initial 'BugsState'
initialBugsState :: BugsState
initialBugsState = BugsState
    { nextBugId = BugId 1
    , bugs      = empty
    }

-- | get the next unused 'BugsId'
genBugId :: Update BugsState BugId
genBugId =
    do bs@BugsState{..} <- get
       put $ bs { nextBugId = BugId $ succ $ unBugId $ nextBugId }
       return nextBugId

-- | get 'Bugs' by 'BugId'
getBugById :: BugId -> Query BugsState (Maybe Bug)
getBugById bid =
    do BugsState{..} <- ask
       return $ getOne (bugs @= bid)

-- | store 'Bugs' in the state. Will overwrite an existing entry with the same 'BugId'
putBug :: Bug -> Update BugsState ()
putBug bug =
    do bs@BugsState{..} <- get
       put $ bs { bugs = updateIx (bugId bug) bug bugs }

allBugIds :: Query BugsState [BugId]
allBugIds =
    do BugsState{..} <- ask
       return $ map bugId (toList bugs)

$(makeAcidic ''BugsState
   [ 'genBugId
   , 'getBugById
   , 'putBug
   , 'allBugIds
   ]
 )
