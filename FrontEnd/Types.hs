{-# LANGUAGE DeriveGeneric #-}

module Common.FrontEnd.Types where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)
import Miso (URI)
import Data.IORef (IORef)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.Thread.Model as Thread
import Common.Network.SiteType (Site)
import Common.FrontEnd.JSONSettings (JSONSettings)

data InitialDataPayload = InitialDataPayload
    { timestamp :: UTCTime
    , initialData :: InitialData
    } deriving (Generic, Eq)

instance FromJSON InitialDataPayload
instance ToJSON InitialDataPayload

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site [ Thread.PostWithBody ]
    | Nil
    deriving (Generic, Eq)

instance FromJSON InitialData
instance ToJSON InitialData

data AppInitCtx = AppInitCtx
    { hydrate :: Bool
    , init_uri :: URI
    , init_settings :: JSONSettings
    , init_payload :: InitialDataPayload
    } deriving Eq

type InitCtxRef = IORef AppInitCtx
