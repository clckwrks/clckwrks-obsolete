{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.SubmitBug where

import Control.Monad.Reader (ask)
import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Data.Monoid (mempty)
import Data.Maybe  (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Set as Set
import HSP
import Text.Reform ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
                   , (<++), prove, transformEither, transform, view)
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform

submitBug :: BugsURL -> BugsM Response
submitBug here =
    do template "Submit a Report" ()
              <%>
               <h1>Submit Report</h1>
               <% reform (form here) "sbr" addReport Nothing submitForm %>
              </%>
    where
      addReport :: Bug -> BugsM Response
      addReport bug =
          do ident <- update GenBugId
             update $ PutBug (bug { bugId = ident })
             seeOtherURL (ViewBug ident)

submitForm :: BugsForm Bug
submitForm =
  fieldset $ ol $
    Bug <$> pure (BugId 0)
        <*> submittorIdForm
        <*> nowForm
        <*> pure New
        <*> pure Nothing
        <*> bugTitleForm
        <*> bugBodyForm
        <*> pure Set.empty
        <*> pure Nothing
        <*  (li $ inputSubmit (pack "submit"))
    where
      submittorIdForm :: BugsForm UserId
      submittorIdForm = impure (fromJust <$> getUserId)

      nowForm :: BugsForm UTCTime
      nowForm = impure (liftIO getCurrentTime)

      bugTitleForm :: BugsForm Text
      bugTitleForm =
          (li $ label (pack "Summary:")) ++> inputText mempty

      bugBodyForm :: BugsForm Markup
      bugBodyForm =
          (li $ label (pack "Details:")) ++> ((\t -> Markup [HsColour, Markdown] t Untrusted) <$> textarea 80 20 mempty)


impure :: (Monoid view, Monad m) => m a -> Form m input error view () a
impure ma =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, do a <- ma
                                             return $ Ok $ Proved { proofs    = ()
                                                                  , pos       = FormRange i i
                                                                  , unProved  = a
                                                                  })


