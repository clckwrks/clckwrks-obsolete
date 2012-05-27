{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Clckwrks.Bugs.PreProcess where
import Control.Monad.Trans
import Control.Applicative
import Clckwrks (ClckT, ClckState)
import Clckwrks.Bugs.URL
import Clckwrks.Bugs.Types (BugId(..))
import Data.Attoparsec.Text
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import HSP
import HSP.HTML (renderAsHTML)
import Web.Routes (showURL)

parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       stringCI name
       skipMany space
       char '='
       skipMany space

parseCmd :: Parser BugId
parseCmd =
    parseAttr "id" *> (BugId <$> decimal)

bugsCmd :: (Functor m, Monad m) => (BugsURL -> [(Text, Maybe Text)] -> Text) -> Text -> ClckT url m Builder
bugsCmd
 showURLFn txt =
    do let mi = parseOnly parseCmd txt
       case mi of
         (Left e) ->
               return $ B.fromString e -- FIXME: format the error more nicely or something?
         (Right bid) ->
             do html <- unXMLGenT $ <a href=(showURLFn (ViewBug bid) [])>#<% show $ unBugId bid  %></a>
                return $ B.fromString $ concat $ lines $ renderAsHTML html

