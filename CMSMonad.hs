{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies, RecordWildCards, ScopedTypeVariables #-}
module CMSMonad
    ( CMS(..)
    , CMSState(..)
    , Content(..)
    , Prefix(..)
    , markupToContent
    , setCurrentPage
    , getPrefix
    , getUnique
    , setUnique
    , query
    , update
    , nestURL
    ) where

import Acid
import Admin.URL (AdminURL(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Acid                     (AcidState, EventState, EventResult, QueryEvent, UpdateEvent)
import Data.Acid.Advanced            (query', update')
import Page.Acid
import Page.Types                    (Markup(..))
import Data.ByteString.Lazy          as LB (ByteString)
import Data.ByteString.Lazy.UTF8     as LB (toString)
import Data.Data
import Data.SafeCopy                 (SafeCopy(..))
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Data.Time.Clock               (UTCTime)
import Data.Time.Format              (formatTime)
import HSP hiding (Request, escape)
import HSP.ServerPartT
import qualified HSX.XMLGenerator as HSX
import Happstack.Server
import System.Locale                 (defaultTimeLocale)
import URL                           (SiteURL(..))
import Web.Routes         hiding (nestURL)
import qualified Web.Routes as R
import Web.Routes.XMLGenT ()
import Web.Routes.Happstack
import Text.Blaze (Html)
import Text.Blaze.Renderer.String (renderHtml)

newtype Prefix = Prefix { prefixText :: T.Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CMSState 
    = CMSState { acidState       :: Acid 
               , currentPage     :: PageId
               , componentPrefix :: Prefix
               , uniqueId        :: Integer
               }

newtype CMS url a = CMS { unCMS :: RouteT url (ServerPartT (StateT CMSState IO)) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, Happstack, ServerMonad, HasRqData, FilterMonad Response, WebMonad Response, MonadState CMSState)

nestURL :: (url1 -> url2) -> CMS url1 a -> CMS url2 a
nestURL f (CMS r) = CMS $ R.nestURL f r

instance MonadRoute (CMS url) where
    type URL (CMS url) = url
    askRouteFn = CMS $ askRouteFn

query :: forall event m. (QueryEvent event, GetAcidState (EventState event), Functor m, MonadIO m, MonadState CMSState m) => event -> m (EventResult event)
query event =
    do as <- (getAcidState . acidState) <$> get
       query' (as :: AcidState (EventState event)) event

update :: forall event m. (UpdateEvent event, GetAcidState (EventState event), Functor m, MonadIO m, MonadState CMSState m) => event -> m (EventResult event)
update event =
    do as <- (getAcidState . acidState) <$> get
       update' (as :: AcidState (EventState event)) event

-- | update the 'currentPage' field of 'CMSState'
setCurrentPage :: PageId -> CMS url ()
setCurrentPage pid =
    modify $ \s -> s { currentPage = pid }

getPrefix :: CMS url Prefix
getPrefix = componentPrefix <$> get

setUnique :: Integer -> CMS url ()
setUnique i =
    modify $ \s -> s { uniqueId = i }

getUnique :: CMS url Integer
getUnique = 
    do s <- get
       let u = uniqueId s
       put $ s { uniqueId = succ u }
       return u

-- * XMLGen / XMLGenerator instances for CMS

instance HSX.XMLGen (CMS url) where
    type HSX.XML (CMS url) = XML
    newtype HSX.Child (CMS url) = CMSChild { unCMSChild :: XML }
    newtype HSX.Attribute (CMS url) = FAttr { unFAttr :: Attribute }
    genElement n attrs children =
        do attribs <- map unFAttr <$> asAttr attrs
           childer <- flattenCDATA . map (unCMSChild) <$> asChild children
           HSX.XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = CMSChild
    pcdataToChild = HSX.xmlToChild . pcdata

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml =
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs                       
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)

instance IsAttrValue (CMS url) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance IsAttrValue (CMS url) TL.Text where
    toAttrValue = toAttrValue . TL.unpack
{-
instance EmbedAsChild CMS (Block t) where
  asChild b = asChild $
    <script type="text/javascript">
      <% show b %>
    </script>

instance IsAttrValue CMS (HJScript (Exp t)) where
  toAttrValue script = toAttrValue $ evaluateHJScript script

instance IsAttrValue CMS (Block t) where
  toAttrValue block = return . attrVal $ "javascript:" ++ show block

instance (IsName n) => HSX.EmbedAsAttr CMS (Attr n (HJScript (Exp a))) where
    asAttr (n := script) = return . (:[]) . FAttr $ MkAttr (toName n, attrVal $ show $ evaluateHJScript script)
-}
instance HSX.EmbedAsAttr (CMS url) Attribute where
    asAttr = return . (:[]) . FAttr 

instance (IsName n) => HSX.EmbedAsAttr (CMS url) (Attr n String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (IsName n) => HSX.EmbedAsAttr (CMS url) (Attr n Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (IsName n) => HSX.EmbedAsAttr (CMS url) (Attr n Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (IsName n) => HSX.EmbedAsAttr (CMS url) (Attr n Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => HSX.EmbedAsAttr (CMS url) (Attr n Integer) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (IsName n) => HSX.EmbedAsAttr (CMS SiteURL) (Attr n SiteURL) where
    asAttr (n := u) = 
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))

instance (IsName n) => HSX.EmbedAsAttr (CMS AdminURL) (Attr n AdminURL) where
    asAttr (n := u) = 
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal (T.unpack url))


{-
instance HSX.EmbedAsAttr CMS (Attr String AuthURL) where
    asAttr (n := u) = 
        do url <- showURL (W_Auth u)
           asAttr $ MkAttr (toName n, pAttrVal url)
-}

instance (IsName n) => (EmbedAsAttr (CMS url) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (IsName n) => (EmbedAsAttr (CMS url) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance EmbedAsChild (CMS url) Char where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata . (:[])

instance EmbedAsChild (CMS url) String where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata

instance EmbedAsChild (CMS url) Int where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata . show

instance EmbedAsChild (CMS url) Integer where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata . show

instance EmbedAsChild (CMS url) Double where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata . show

instance EmbedAsChild (CMS url) Float where
    asChild = XMLGenT . return . (:[]) . CMSChild . pcdata . show

instance EmbedAsChild (CMS url) TL.Text where
    asChild = asChild . TL.unpack

instance EmbedAsChild (CMS url) T.Text where
    asChild = asChild . T.unpack

instance (EmbedAsChild (CMS url1) a, url1 ~ url2) => EmbedAsChild (CMS url1) (CMS url2 a) where
    asChild c = 
        do a <- XMLGenT c
           asChild a

instance (EmbedAsChild (CMS url) a) => EmbedAsChild (CMS url) (IO a) where
    asChild c = 
        do a <- XMLGenT (liftIO c)
           asChild a

{-
instance EmbedAsChild CMS TextHtml where
    asChild = XMLGenT . return . (:[]) . CMSChild . cdata . T.unpack . unTextHtml
-}
instance EmbedAsChild (CMS url) XML where
    asChild = XMLGenT . return . (:[]) . CMSChild

instance EmbedAsChild (CMS url) Html where
    asChild = XMLGenT . return . (:[]) . CMSChild . cdata . renderHtml

instance EmbedAsChild (CMS url) Markup where
    asChild mrkup = asChild =<< markupToContent mrkup

instance EmbedAsChild (CMS url) () where
    asChild () = return []

instance EmbedAsChild (CMS url) UTCTime where
    asChild = asChild . formatTime defaultTimeLocale "%a, %F @ %r"

instance AppendChild (CMS url) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unCMSChild chs))

instance SetAttr (CMS url) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unFAttr attrs)) cs

instance XMLGenerator (CMS url)

data Content 
    = TrustedHtml T.Text
    | PlainText   T.Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance EmbedAsChild (CMS url) Content where
    asChild (TrustedHtml html) = asChild $ cdata (T.unpack html)
    asChild (PlainText txt)    = asChild $ pcdata (T.unpack txt)

markupToContent :: (MonadIO m) => Markup -> m Content
markupToContent Markup{..} =
    do e <- runPreProcessors preProcessors markup
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

