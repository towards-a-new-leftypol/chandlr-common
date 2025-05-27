{-# LANGUAGE ExistentialQuantification #-}

module Common.FrontEnd.Action where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (URI)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Network.HttpTypes (HttpResult)
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
    = GetThread GetThreadArgs
    | HaveLatest (HttpResult [ CatalogPost ])
    | HaveThread (HttpResult [ Site ])
    -- | forall n m a b. (FromJSON a) => ClientAction (HttpResult a -> Action) (C.Action n m a b)
    | ThreadAction Thread.Action
    | TimeAction TC.Time
    | SearchAction Search.Action
    | GoToTime UTCTime
    | ChangeURI URI
    | SearchResults MisoString
    | NoAction


mkGetThread :: CatalogPost -> Action
mkGetThread post = GetThread GetThreadArgs
    { website = CatalogPost.site_name post
    , board_pathpart = CatalogPost.pathpart post
    , board_thread_id = CatalogPost.board_thread_id post
    }
