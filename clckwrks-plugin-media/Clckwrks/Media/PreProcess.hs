{-# LANGUAGE OverloadedStrings #-}
module Clckwrks.Media.PreProcess where
import Control.Monad.Trans
import Control.Applicative
import Clckwrks (ClckT, ClckState)
import Clckwrks.Media.URL
import Clckwrks.Media.Types (MediumId(..))
import Data.Attoparsec.Text
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import           Text.Blaze.Html ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Routes (showURL)

parseAttr :: Text -> Parser ()
parseAttr name =
    do skipMany space
       stringCI name
       skipMany space
       char '='
       skipMany space

width :: Parser H.Attribute
width =
    A.width . H.toValue <$> (parseAttr "width" *> (decimal :: Parser Integer))

height :: Parser H.Attribute
height =
    A.height . H.toValue <$> (parseAttr "height" *> (decimal :: Parser Integer))

parseCmd :: Parser (MediumId, [H.Attribute])
parseCmd =
    (,) <$> (parseAttr "id" *> (MediumId <$> decimal))
        <*> (many $ choice [ width, height ])

mediaCmd :: (Monad m) => (MediaURL -> [(Text, Maybe Text)] -> Text) -> Text -> ClckT url m Builder
mediaCmd showURLFn txt =
    do let mi = parseOnly parseCmd txt
       case mi of
         (Left e) ->
               return $ B.fromString e -- FIXME: format the error more nicely or something?
         (Right (mid, attrs)) ->
             do let u = toValue $ showURLFn (GetMedium mid) []
                return $ B.fromLazyText $ renderHtml $ foldr (\attr tag -> tag ! attr) H.img (A.src u : attrs)
