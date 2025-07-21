{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}

module Common.Network.ClientTypes where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Aeson (ToJSON, FromJSON, Result)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (Topic, topic)

import qualified Common.Network.HttpTypes as Http

data Action
    = Connect Sender Http.HttpActionResult
    | OnMessage (Result MessageIn)
    | Publish MessageOut
    | OnMount
    | OnUnmount
    | Initialize

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

type Sender = MisoString

type MessageIn = (Sender, Query)

data Query
    = FetchLatest UTCTime
    | GetThread GetThreadArgs
    | Search MisoString
    | InitModel Model
    deriving (Generic, ToJSON, FromJSON)

data MessageOut
    = ReturnResult Sender Http.HttpResult
    | Mounted
    | Unmounted
    deriving (Eq, Generic, ToJSON, FromJSON)

clientInTopic :: Topic MessageIn
clientInTopic = topic "client-in"

clientOutTopic :: Topic MessageOut
clientOutTopic = topic "client-out"


data GetThreadArgs = GetThreadArgs
    { website         :: Text
    , board_pathpart  :: Text
    , board_thread_id :: Int64
    } deriving (Eq, Generic, ToJSON, FromJSON)
