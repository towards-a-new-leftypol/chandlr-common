{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Miso (Topic, topic, Component)
import Miso.String (MisoString)
import Miso.Lens (Lens, LensCore (..))
import Common.MisoAeson
import Miso.JSON qualified

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

getSetMediaRoot :: Lens Model MisoString
getSetMediaRoot =
    Lens
        media_root
        (\x model -> model { media_root = x })

type GridComponent parent = Component parent Model Action

newtype Action
    = ThreadSelected CatalogPost

newtype OutMessage
    = SelectThread CatalogPost
    deriving stock (Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)
    deriving (Miso.JSON.ToJSON, Miso.JSON.FromJSON) via (MisoAeson OutMessage)

catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"
