{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.EditBug where

import Control.Arrow        (first)
import Control.Monad.Reader (ask)
import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.ProfileData.Acid (GetUserIdUsernames(..))
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import HSP
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view
                   )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform

editBug :: BugsURL -> BugId -> BugsM Response
editBug here bid =
    do mBug <- query (GetBugById bid)
       case mBug of
         Nothing ->
             do notFound ()
                template "Bug not found." ()
                         <h1>BugId Not Found: <% bid %></h1>
         (Just bug) ->
          do users      <- getUsers
             milestones <- query $ GetMilestones
             template "Edit Bug Report" ()
              <%>
               <h1>Edit Bug Report</h1>
               <% reform (form here) "sbr" updateReport Nothing (editBugForm users milestones bug) %>
              </%>
    where
      updateReport :: Bug -> BugsM Response
      updateReport bug =
          do update $ PutBug bug
             seeOtherURL (ViewBug bid)

      getUsers :: BugsM [(Maybe UserId, Text)]
      getUsers =
          ((Nothing, pack "Unassigned") :) . map (first Just) <$> query GetUserIdUsernames


editBugForm :: [(Maybe UserId, Text)] -> [Milestone] -> Bug -> BugsForm Bug
editBugForm users milestones bug@Bug{..} =
  (fieldset $ ol $
    Bug <$> pure bugId
        <*> pure bugSubmittor
        <*> pure bugSubmitted
        <*> bugStatusForm bugStatus
        <*> bugAssignedForm bugAssigned
        <*> bugTitleForm bugTitle
        <*> bugBodyForm bugBody
        <*> pure Set.empty
        <*> bugMilestoneForm bugMilestone
        <*  (li $ inputSubmit (pack "update")))
   `setAttrs` ["class" := "bugs"]
    where

      bugStatusForm :: BugStatus -> BugsForm BugStatus
      bugStatusForm oldStatus =
          (li $ label (pack "Status:")) ++> select [(s, show s) | s <- [minBound .. maxBound]] (== oldStatus)

      bugAssignedForm :: Maybe UserId -> BugsForm (Maybe UserId)
      bugAssignedForm mUid =
          (li $ label (pack "Assigned:")) ++>
            select users (== mUid)

      bugTitleForm :: Text -> BugsForm Text
      bugTitleForm oldTitle =
          (li $ label (pack "Summary:")) ++> (inputText oldTitle `setAttrs` ["size" := "80"])

      bugBodyForm :: Markup -> BugsForm Markup
      bugBodyForm oldBody =
          (li $ label (pack "Details:")) ++> ((\t -> Markup [HsColour, Markdown] t Untrusted) <$> textarea 80 20 (markup oldBody))

      bugMilestoneForm :: Maybe MilestoneId -> BugsForm (Maybe MilestoneId)
      bugMilestoneForm mMilestone =
          (li $ label (pack "Milestone:")) ++>
            select ((Nothing, pack "none") : [(Just $ milestoneId m, milestoneTitle m) | m <- milestones]) (== mMilestone)


impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })
