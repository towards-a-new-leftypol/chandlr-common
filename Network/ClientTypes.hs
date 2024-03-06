module Common.Network.ClientTypes where

import qualified Network.Http as Http
import Miso.String (MisoString)

data Action a = Connect (Http.HttpActionResult a)

data Interface a b = Interface
    { passAction :: Action b -> a
    , returnResult :: Http.HttpResult b -> a
    }

data Model = Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq


