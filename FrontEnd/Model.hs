module Common.FrontEnd.Model where

import Miso (URI)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)

import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.ThreadView as Thread
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Component.TimeControl as TC
import qualified Common.Network.ClientTypes as Client


data Model = Model
    { grid_model :: Grid.Model
    , client_model :: Client.Model
    , thread_model :: Maybe Thread.Model
    , current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: UTCTime
    , tc_model :: TC.Model
    , search_model :: Search.Model
    } deriving Eq
