{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Component.CatalogGrid.GridTypes where

import GHC.Generics (Generic)
import Miso.JSON (ToJSON, FromJSON)
import Miso (Topic, topic, Component)
import Miso.String (MisoString)

import Common.Network.CatalogPostType (CatalogPost)

data Props f = Props
  { display_items :: f CatalogPost
  , media_root :: MisoString
  }

deriving stock instance Eq (f CatalogPost) => Eq (Props f)

type Model = ()

type GridComponent context f = Component context (Props f) Model Action

newtype Action
    = ThreadSelected CatalogPost

newtype OutMessage
    = SelectThread CatalogPost
    deriving stock (Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)

catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"
