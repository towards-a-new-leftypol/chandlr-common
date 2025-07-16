{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Result(..))
import Miso (Topic, topic, Component)
import Miso.String (MisoString)

import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.ClientTypes (GetThreadArgs)

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

type GridComponent = Component Model Action

data Action
    = ThreadSelected CatalogPost
    | OnMessage (Result InMessage)
    | Initialize

data OutMessage
    = GetThread GetThreadArgs
    deriving (Eq, Generic, ToJSON, FromJSON)

data InMessage
    = DisplayItems [ CatalogPost ]
    deriving (Generic, ToJSON, FromJSON)


catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"


catalogInTopic :: Topic InMessage
catalogInTopic = topic "catalog-in"
