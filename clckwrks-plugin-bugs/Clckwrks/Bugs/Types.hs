{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.Bugs.Types where

import Clckwrks
import Data.Data     (Data, Typeable)
import Data.IxSet    (Indexable(..), ixSet, ixFun)
import Data.Maybe    (maybeToList)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text)
import Data.Time     (UTCTime)
import Data.Set      (Set)
import Web.Routes    (PathInfo(..))

newtype BugId = BugId { unBugId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, PathInfo)

newtype BugTag = BugTag { tagText :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, PathInfo)

newtype MilestoneId = MilestoneId { unMilestoneId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, PathInfo)

data BugStatus
    = New
    | Accepted
    | Closed
    | Invalid
    | WontFix
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''BugStatus)

data Bug
    = Bug { bugId        :: BugId
          , bugSubmittor :: UserId
          , bugSubmitted :: UTCTime
          , bugStatus    :: BugStatus
          , bugAssigned  :: Maybe UserId
          , bugTitle     :: Text
          , bugBody      :: Markup
          , bugTags      :: Set BugTag
          , bugMilestone :: Maybe MilestoneId
          }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Bug)

instance Indexable Bug where
    empty = ixSet [ ixFun ((:[]) . bugId)
                  , ixFun (maybeToList . bugMilestone)
                  ]
