{-# LANGUAGE ExistentialQuantification #-}

module Common.FrontEnd.Action where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.JSString (JSString)
import Miso (URI)

import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Network.ClientTypes as C
import Common.Network.CatalogPostType (CatalogPost)
import Network.Http (HttpResult)
import Common.Network.SiteType (Site)
import qualified Common.Component.ThreadView as Thread
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.Search.SearchTypes as Search

data GetThreadArgs = GetThreadArgs
    { website         :: Text
    , board_pathpart  :: Text
    , board_thread_id :: Int64
    }

data Action
    = GridAction Grid.Action
    | GetThread GetThreadArgs
    | HaveLatest (HttpResult [ CatalogPost ])
    | HaveThread (HttpResult [ Site ])
    | forall a. (FromJSON a) => ClientAction (HttpResult a -> Action) (C.Action a)
    | ThreadAction Thread.Action
    | TimeAction TC.Time
    | SearchAction Search.Action
    | GoToTime UTCTime
    | ChangeURI URI
    | SearchResults JSString
    | NoAction
