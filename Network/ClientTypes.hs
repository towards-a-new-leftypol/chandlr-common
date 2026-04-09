{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

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
    | Initialize

data Model = Uninitialized | Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving (Eq, Show)

instance ToJSON Model where
    toJSON Uninitialized = object [ "tag" .= String "Uninitialized" ]
    toJSON Model {..} =
        object
            [ "tag"        .= String "Model"
            , "pgApiRoot"  .= String pgApiRoot
            , "fetchCount" .= Number (fromIntegral fetchCount)
            ]

instance FromJSON Model where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString

        case tag of
            "Uninitialized" -> pure Uninitialized

            "Model"         -> Model <$> m .: "pgApiRoot"
                                     <*> m .: "fetchCount"

            _               -> fail "Unknown Model tag"

    parseJSON _ = fail "Expected Object for HttpResult"


data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving Generic

instance ToJSON FetchCatalogArgs where
    toJSON (FetchCatalogArgs {..}) =
        object
            [ "max_time"     .= toJSON max_time
            , "max_row_read" .= Number (fromIntegral max_row_read)
            ]

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
    = FetchLatest UTCTime
    | GetThread GetThreadArgs
    | Search MisoString
    | DeleteIllegalPost DeleteIllegalPostArgs
    | InitModel Model
    deriving Eq

instance ToJSON Query where
    toJSON (FetchLatest t) = object
        [ "tag"  .= String "FetchLatest"
        , "args" .= toJSON t
        ]
    toJSON (GetThread a) = object
        [ "tag"  .= String "GetThread"
        , "args" .= toJSON a
        ]
    toJSON (Search q) = object
        [ "tag"  .= String "Search"
        , "args" .= String q
        ]
    toJSON (DeleteIllegalPost a) = object
        [ "tag"  .= String "DeleteIllegalPost"
        , "args" .= toJSON a
        ]
    toJSON (InitModel m) = object
        [ "tag"  .= String "InitModel"
        , "args" .= toJSON m
        ]

instance FromJSON Query where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString
        case tag of
            "FetchLatest"         -> FetchLatest       <$> m .: "args"
            "GetThread"           -> GetThread         <$> m .: "args"
            "Search"              -> Search            <$> m .: "args"
            "DeleteIllegalPost"   -> DeleteIllegalPost <$> m .: "args"
            "InitModel"           -> InitModel         <$> m .: "args"
            _                     -> fail "Unknown Query tag"

    parseJSON _ = fail "Expected Object for Query"


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
