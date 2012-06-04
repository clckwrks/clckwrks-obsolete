{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.ViewBug where

import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Page.Template (template)
import Clckwrks.ProfileData.Acid
import Data.Maybe (fromMaybe, maybe)
import Data.Set   (Set)
import qualified Data.Set as Set
import Data.Text  (pack)

import Happstack.Auth (AuthState, ProfileState)
import Control.Monad.State

viewBug :: BugId -> BugsM Response
viewBug bid =
    do mBug <- query (GetBugById bid)
       case mBug of
         Nothing -> do notFound ()
                       template "bug not found." ()
                        <p>Could not find Bug #<% show $ unBugId bid %></p>
         (Just bug) -> bugHtml bug

bugHtml :: Bug -> BugsM Response
bugHtml Bug{..} =
    do submittor       <- query (GetUsername bugSubmittor)
       milestoneTxt <-
           case bugMilestone of
             Nothing  -> return (pack "none")
             Just mid ->
                 fromMaybe (pack $ show mid) <$> query (GetMilestoneTitle mid)
       template ("Bug #" ++ (show $ unBugId bugId)) ()
         <%>
           <dl id="view-bug">
            <dt>Bug #:</dt>       <dd><% show $ unBugId bugId %></dd>
            <dt>Submitted By:</dt><dd><% fromMaybe (pack "Anonymous") submittor %></dd>
            <dt>Submitted:</dt>   <dd><% bugSubmitted %></dd>
            <dt>Status:</dt>      <dd><% show bugStatus %></dd>
            <dt>Milestone:</dt>   <dd><% milestoneTxt %></dd>
            <dt>Title:</dt>       <dd><% bugTitle %></dd>
            <dt>Body:</dt>        <dd><% bugBody %></dd>
            <% whenHasRole (Set.singleton Administrator) <a href=(BugsAdmin (EditBug bugId))>edit</a> %>
           </dl>
         </%>

whenHasRole :: (Happstack m, GetAcidState m AuthState
               , GetAcidState m ProfileState
               , GetAcidState m ProfileDataState
               , MonadState ClckState m
               ) =>
     Set Role -> m XML -> m XML
whenHasRole role xml =
    do muid <- getUserId
       case muid of
         (Just uid) ->
             do b <- query (HasRole uid role)
                if b
                  then xml
                  else return $ cdata ""
         _ -> return $ cdata ""
