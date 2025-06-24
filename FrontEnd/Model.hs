module Common.FrontEnd.Model where

import Miso (URI)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)

data Model = Model
    { current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: UTCTime
    , searchTerm :: MisoString
    } deriving Eq
