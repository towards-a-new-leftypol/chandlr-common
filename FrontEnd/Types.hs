{-# LANGUAGE DeriveGeneric #-}

module Common.FrontEnd.Types where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.Thread as Thread
import Common.Network.SiteType (Site)

data InitialDataPayload = InitialDataPayload
    { timestamp :: UTCTime
    , initialData :: InitialData
    } deriving (Generic)

instance FromJSON InitialDataPayload
instance ToJSON InitialDataPayload

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site [ Thread.PostWithBody ]
    | Nil
    deriving (Generic)

instance FromJSON InitialData
instance ToJSON InitialData
