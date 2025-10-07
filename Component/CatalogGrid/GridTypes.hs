{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Miso (Topic, topic, Component)
import Miso.String (MisoString)
import Miso.Lens (Lens (..))

import Common.Network.CatalogPostType (CatalogPost)

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

getSetDisplayItems :: Lens Model [ CatalogPost ]
getSetDisplayItems =
    Lens
        display_items
        (\x model -> model { display_items = x })

type GridComponent parent = Component parent Model Action

data Action
    = ThreadSelected CatalogPost

newtype OutMessage
    = SelectThread CatalogPost
    deriving (Eq, Generic)

instance ToJSON OutMessage
instance FromJSON OutMessage

catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"
