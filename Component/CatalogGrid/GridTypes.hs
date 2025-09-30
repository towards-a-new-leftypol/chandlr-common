{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Miso (Topic, topic, Component)
import Miso.String (MisoString)

import Common.Network.CatalogPostType (CatalogPost)

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

type GridComponent parent = Component parent Model Action

data Action
    = ThreadSelected CatalogPost
    | OnMessage InMessage
    | OnMessageError MisoString
    | Initialize

newtype OutMessage
    = SelectThread CatalogPost
    deriving (Eq, Generic)

instance ToJSON OutMessage
instance FromJSON OutMessage

newtype InMessage
    = DisplayItems [ CatalogPost ]
    deriving (Generic)

instance ToJSON InMessage
instance FromJSON InMessage


catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"


catalogInTopic :: Topic InMessage
catalogInTopic = topic "catalog-in"
