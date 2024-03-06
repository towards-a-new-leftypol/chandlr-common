module Common.Network.ClientTypes where

import qualified Network.Http as Http
import Miso.String (MisoString)

data Action a = Connect (Http.HttpActionResult a)

data Model = Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq


