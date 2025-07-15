module Common.FrontEnd.Model where

import Miso (URI, ComponentId)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Common.FrontEnd.Action (Action)
import qualified Common.Component.Thread  as Thread

data Model = Model
    { current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: UTCTime
    , search_term :: MisoString
    , initial_action :: Action
    , thread_message :: Maybe Thread.Message
    , pg_api_root :: MisoString
    , client_fetch_count :: Int
    , my_component_id :: ComponentId
    } deriving Eq
