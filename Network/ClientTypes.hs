{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Common.Network.ClientTypes where

import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (Topic, topic)
import Miso.JSON

import qualified Common.Network.HttpTypes as Http
import Common.Utils ()

data Action
    = Connect ReturnTopicName Http.HttpActionResult
    | OnMessage MessageIn
    | OnErrorMessage MisoString
    | Publish ReturnTopicName MessageOut
    | PropsInitialized
    | Initialize

data Model = Model
    { initialized :: Bool
    , messageQueue :: [ MessageIn ]
    } deriving Eq

data Props = Props
  { pgApiRoot :: MisoString
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FetchCatalogArgs = FetchCatalogArgs
  { selected_time    :: UTCTime
  , scroll_time      :: Maybe UTCTime
  , scroll_thread_id :: Maybe Integer
  , thread_count     :: Int
  , board_ids        :: Maybe [ Int ]
  } deriving (Eq, Generic, ToJSON, FromJSON)


data SearchPostsArgs = SearchPostsArgs
  { search_text :: MisoString
  , max_rows :: Int
  } deriving (Generic, ToJSON)

type ReturnTopicName = MisoString
-- TODO: change Sender to be a Topic is this possible? It should be...
-- just create a
-- searchClientResponseTopic :: Topic Client.MessageOut
-- searchClientResponseTopic = topic "search-client-response"
--
-- for every component using Client

type MessageIn = (ReturnTopicName, Query)


data Query
    = FetchLatest FetchCatalogArgs
    | GetThread GetThreadArgs
    | Search MisoString
    | DeleteIllegalPost DeleteIllegalPostArgs
    | LoadAllSitesAndBoards
    deriving (Eq, Generic, ToJSON, FromJSON)


newtype MessageOut = ReturnResult Http.HttpResult
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

clientInTopic :: Topic MessageIn
clientInTopic = topic "client-in"

data GetThreadArgs = GetThreadArgs
    { website         :: MisoString
    , board_pathpart  :: MisoString
    , board_thread_id :: Integer
    }
    deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DeleteIllegalPostArgs = DeleteIllegalPostArgs
    { post_id :: Integer
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
