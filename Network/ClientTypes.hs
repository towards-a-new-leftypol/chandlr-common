{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.ClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (Component)

import qualified Common.Network.HttpTypes as Http
import qualified Common.FrontEnd.Action as A

type Action n m a b = (Interface n m a b, ActionVerb b)

data ActionVerb b
    = Connect (Http.HttpActionResult b)
    | FetchLatest UTCTime
    | GetThread A.GetThreadArgs
    | Search MisoString

data Model = Uninitialized | Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq

data Interface n m a b = Interface
    { returnResult :: Http.HttpResult b -> a
    , notifyComponent :: Component n m a
    }

data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving (Generic, ToJSON)


data SearchPostsArgs = SearchPostsArgs
  { search_text :: MisoString
  , max_rows :: Int
  } deriving (Generic, ToJSON)


