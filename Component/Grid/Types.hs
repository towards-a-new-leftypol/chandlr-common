{-# LANGUAGE DataKinds #-}

module Common.Component.Grid.Types where

import Miso
    ( Component
    )
import Miso.String (MisoString)

import Common.Network.CatalogPostType (CatalogPost)

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

type GridComponent = Component "catalog-grid" Model Action

data Action
    = DisplayItems [ CatalogPost ]
    | ThreadSelected CatalogPost
    deriving Eq


data Interface a = Interface
    { passAction :: Action -> a -- We're not using this.
    , threadSelected :: CatalogPost -> a
    }
