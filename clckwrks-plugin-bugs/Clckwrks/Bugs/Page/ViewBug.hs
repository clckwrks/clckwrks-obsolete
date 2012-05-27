{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.Page.ViewBug where

import Clckwrks
import Clckwrks.Bugs.Acid
import Clckwrks.Bugs.Monad
import Clckwrks.Bugs.Types
import Clckwrks.Bugs.Page.Template (template)

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
    template ("Bug #" ++ (show $ unBugId bugId)) ()
     <dl id="view-bug">
         <dt>Bug #</dt><dd><% show $ unBugId bugId %></dd>
         <dt>Submitted By:</dt><dd><% bugId %></dd>
     </dl>
