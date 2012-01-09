{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Media.PreProcess where

import Control.Applicative
import Clckwrks (ClckT, ClckState)
import Clckwrks.Media.URL
import Clckwrks.Media.Types (MediumId(..))
import Data.Attoparsec.Text
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import           Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Text (renderHtml)
import Web.Routes (showURL)



-- we need to be able to access the acid database here and also generate URLs here.
-- So, we need to be in a cooler monad.
mediaCmd_ :: (MediaURL -> [(Text, Maybe Text)] -> Text) -> ClckState -> Text -> IO Builder
mediaCmd_ showURLFn _clckState txt =
    do let mi = parseOnly (string "id=" >> signed decimal) txt
       case mi of
         (Left e) -> undefined
         (Right i) ->
             do let u = toValue $ showURLFn (GetMedium (MediumId i)) []
                return $ B.fromLazyText $ renderHtml $ H.img ! A.src u
       

mediaCmd :: (Monad m) => (MediaURL -> [(Text, Maybe Text)] -> Text) -> Text -> ClckT url m Builder
mediaCmd showURLFn txt =
    do let mi = parseOnly (string "id=" >> signed decimal) txt
       case mi of
         (Left e) ->
               return $ B.fromString e -- FIXME: format the error more nicely or something?
         (Right i) ->
             do let u = toValue $ showURLFn (GetMedium (MediumId i)) []
                return $ B.fromLazyText $ renderHtml $ H.img ! A.src u
       