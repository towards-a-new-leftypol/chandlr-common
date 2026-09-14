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

data Props f g = Props
  { display_items :: f (g CatalogPost)
  , media_root :: MisoString
  }

deriving stock instance (Eq (f (g CatalogPost))) => Eq (Props f g)

type Model = ()

type GridComponent context f g = Component context (Props f g) Model Action

newtype Action
    = ThreadSelected CatalogPost

newtype OutMessage
    = SelectThread CatalogPost
    deriving stock (Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)

catalogOutTopic :: Topic OutMessage
catalogOutTopic = topic "catalog-out"
