{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.ClientTypes where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (ComponentId)

import qualified Common.Network.HttpTypes as Http

data Action
    = Connect Sender Http.HttpActionResult
    | OnMessage MessageIn
    | OnErrorMessage MisoString

data Model = Uninitialized | Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving (Eq, Generic, ToJSON, FromJSON)

data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving (Generic, ToJSON)

data SearchPostsArgs = SearchPostsArgs
  { search_text :: MisoString
  , max_rows :: Int
  } deriving (Generic, ToJSON)

type Sender = ComponentId

type ReturnEnvelope = forall a. (ToJSON a) => Http.HttpResult -> a

type MessageIn = 
    (Sender, ReturnEnvelope, Query)

data Query
    = FetchLatest UTCTime
    | GetThread GetThreadArgs
    | Search MisoString
    | InitModel Model
    deriving (Generic, ToJSON, FromJSON)

data GetThreadArgs = GetThreadArgs
    { website         :: MisoString
    , board_pathpart  :: MisoString
    , board_thread_id :: Int64
    } deriving (Eq, Generic, ToJSON, FromJSON)
