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

data Action n m a b
    = Connect (Interface n m a b) (Http.HttpActionResult b)
    | FetchLatest UTCTime (Interface n m a b)
    | GetThread A.GetThreadArgs (Interface n m a b)
    | Search MisoString (Interface n m a b)

data Model = Model
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


