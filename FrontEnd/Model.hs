module Common.FrontEnd.Model where

import Miso (URI)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Common.FrontEnd.Action (Action)
import qualified Common.Component.Thread  as Thread
import qualified Common.Component.Grid.Types as Grid

data Model = Model
    { current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: UTCTime
    , search_term :: MisoString
    , initial_action :: Action
    , thread_action :: Maybe Thread.Action
    , grid_action :: Maybe Grid.Action
    , pg_api_root :: MisoString
    , client_fetch_count :: Int
    } deriving Eq
