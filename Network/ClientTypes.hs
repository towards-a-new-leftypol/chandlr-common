module Common.Network.ClientTypes where

import qualified Common.Network.HttpTypes as Http
import Miso.String (MisoString)

data Action a = Connect (Http.HttpActionResult a)

data Model = Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq

data Interface a b = Interface
    { passAction :: Action b -> a
    , returnResult :: Http.HttpResult b -> a
    }

data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving (Generic, ToJSON)


data SearchPostsArgs = SearchPostsArgs
  { search_text :: JSString
  , max_rows :: Int
  }
    deriving (Generic, ToJSON)


