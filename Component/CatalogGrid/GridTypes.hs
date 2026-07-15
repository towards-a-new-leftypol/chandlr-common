{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Miso.JSON (ToJSON, FromJSON)
import Miso (Topic, topic, Component)
import Miso.String (MisoString)

import Common.Network.CatalogPostType (CatalogPost)

data Props = Props
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

type Model = ()

type GridComponent parent = Component parent Props Model Action

newtype Action
    = ThreadSelected CatalogPost

newtype OutMessage
    = SelectThread CatalogPost
    deriving stock (Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)

catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"
