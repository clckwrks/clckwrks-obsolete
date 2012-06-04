{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Bugs.Acid where

import Control.Applicative    ((<$>))
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, modify, put)
import Data.Acid              (Query, Update, makeAcidic)
import Data.IxSet             (IxSet, Proxy(..), (@=), (@+), getOne, empty, toAscList, toList, fromList, updateIx)
import qualified Data.IxSet   as IxSet
import Data.Map               (Map)
import qualified Data.Map     as Map
import Data.Ratio             ((%))
import Data.SafeCopy          (base, deriveSafeCopy, extension, Migrate(..))
import           Data.Text    (Text)
import qualified Data.Text    as Text
import Clckwrks.Bugs.Types    (Bug(..), BugStatus(..), BugId(..), Milestone(..), MilestoneId(..), TargetDate(..))

data BugsState_0 = BugsState_0
    { nextBugId_0       :: BugId
    , bugs_0            :: IxSet Bug
    }
$(deriveSafeCopy 0 'base ''BugsState_0)

-- | 'BugsState' stores all the bugs
data BugsState = BugsState
    { nextBugId       :: BugId
    , bugs            :: IxSet Bug
    , nextMilestoneId :: MilestoneId
    , milestones      :: IxSet Milestone
    }
$(deriveSafeCopy 1 'extension ''BugsState)

instance Migrate BugsState where
    type MigrateFrom BugsState = BugsState_0
    migrate (BugsState_0 n b) = BugsState n b (MilestoneId 1) empty

-- | initial 'BugsState'
initialBugsState :: BugsState
initialBugsState = BugsState
    { nextBugId       = BugId 1
    , bugs            = empty
    , nextMilestoneId = MilestoneId 1
    , milestones      = empty
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

------------------------------------------------------------------------------
-- Milestones
------------------------------------------------------------------------------

-- | add a new, empty 'Milestone' to the database and return the 'MilestoneId'
newMilestone :: Update BugsState MilestoneId
newMilestone =
    do bs@BugsState{..} <- get
       let milestone = Milestone { milestoneId      = nextMilestoneId
                                 , milestoneTitle   = Text.empty
                                 , milestoneTarget  = Nothing
                                 , milestoneReached = Nothing
                                 }
       put $ bs { nextMilestoneId = succ nextMilestoneId
                , milestones      = IxSet.insert milestone milestones
                }
       return nextMilestoneId

-- | get the milestones
getMilestones :: Query BugsState [Milestone]
getMilestones =
    do ms <- milestones <$> ask
       return (toList ms)

-- | get all the 'MilestoneId's
getMilestoneIds :: Query BugsState [MilestoneId]
getMilestoneIds =
    do ms <- milestones <$> ask
       return (map milestoneId $ toList ms)

-- | get the 'milestoneTitle' for a 'MilestoneId'
getMilestoneTitle :: MilestoneId -> Query BugsState (Maybe Text)
getMilestoneTitle mid =
    do ms <- milestones <$> ask
       return $ milestoneTitle <$> getOne (ms @= mid)

-- | get the milestones sorted by target date
setMilestones :: [Milestone] -> Update BugsState ()
setMilestones ms =
    modify $ \bs -> bs { milestones = fromList ms }

-- | get all the 'Bug's with one of the target 'MilestoneId's
bugsForMilestones :: [MilestoneId] -> Query BugsState (IxSet Bug)
bugsForMilestones mids =
    do bs <- bugs <$> ask
       return $ (bs @+ mids)

-- | return the percentage completion of a 'MilestoneId'
--
-- Will return 'Nothing' if no bugs were found for the 'MilestoneId'
milestoneCompletion :: MilestoneId
                    -> Query BugsState (Maybe Rational)
milestoneCompletion mid =
    do bs <- IxSet.getEQ mid . bugs <$> ask
       case IxSet.size bs of
         0     -> return Nothing
         total -> let closed = IxSet.size (bs @+ [Closed, Invalid, WontFix])
                  in return $ Just (toRational (closed % total))

$(makeAcidic ''BugsState
   [ 'genBugId
   , 'getBugById
   , 'putBug
   , 'allBugIds
   , 'newMilestone
   , 'getMilestones
   , 'getMilestoneTitle
   , 'setMilestones
   , 'bugsForMilestones
   , 'milestoneCompletion
   ]
 )
