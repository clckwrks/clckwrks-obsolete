{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Page.Acid 
    ( module Page.Types
      -- * state
    , PageState
    , initialPageState
      -- * events
    , PageById(..)
    , PagesSummary(..)
    , UpdatePage(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable, IxSet, (@=), empty, fromList, getOne, ixSet, ixFun, toList, updateIx)
import Data.SafeCopy
import Data.Text (Text, pack)
import Page.Types

data PageState  = PageState 
    { nextPageId :: PageId
    , pages      :: Pages 
    }
    deriving (Eq, Read, Show, Data, Typeable)
$(deriveSafeCopy 1 'base ''PageState)

initialPageState :: PageState
initialPageState = 
    PageState { nextPageId = PageId 2
              , pages = fromList [ Page { pageId    = PageId 1
                                        , pageTitle = pack "This title rocks!"
                                        , pageSrc   = Markdown $ pack "This is the body!"
                                        } 
                                 ]
              }

pageById :: PageId -> Query PageState (Maybe Page)
pageById pid =
    do pgs <- pages <$> ask
       return $ getOne $ pgs @= pid

pagesSummary :: Query PageState [(PageId, Text)]
pagesSummary =
    do pgs <- pages <$> ask
       return $ map (\page -> (pageId page, pageTitle page)) (toList pgs)

updatePage :: Page -> Update PageState (Maybe String)
updatePage page =
    do ps@PageState{..} <- get
       case getOne $ pages @= (pageId page) of
         Nothing  -> return $ Just $ "updatePage: Invalid PageId " ++ show (unPageId $ pageId page)
         (Just _) -> 
             do put $ ps { pages = updateIx (pageId page) page pages }
                return Nothing

$(makeAcidic ''PageState 
  [ 'pageById
  , 'pagesSummary
  , 'updatePage
  ])
