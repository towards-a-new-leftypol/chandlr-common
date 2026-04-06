{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Types where

import GHC.Generics
import Miso.JSON
import Miso.String (MisoString)
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
    } deriving (Eq, Generic, FromJSON, ToJSON)

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site [ Thread.PostWithBody ]
    | Nil
    deriving (Eq)

instance ToJSON InitialData where
    toJSON (CatalogData posts) = object
        [ "tag"  .= String "CatalogData"
        , "args" .= toJSON posts
        ]
    toJSON (SearchData posts)  = object
        [ "tag"  .= String "SearchData"
        , "args" .= toJSON posts
        ]
    toJSON (ThreadData site posts) = object
        [ "tag"  .= String "ThreadData"
        , "args" .= object [ "site" .= toJSON site, "posts" .= toJSON posts ]
        ]
    toJSON Nil = object [ "tag" .= String "Nil" ]

instance FromJSON InitialData where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString
        case tag of
            "CatalogData"  -> CatalogData <$> m .: "args"
            "SearchData"   -> SearchData  <$> m .: "args"
            "ThreadData"   -> do
                argsObj <- (m .: "args") :: Parser Object
                ThreadData <$> argsObj .: "site" <*> argsObj .: "posts"
            "Nil"          -> pure Nil
            _              -> fail "Unknown InitialData tag"
    parseJSON _ = fail "Expected Object for InitialData"

data AppInitCtx = AppInitCtx
    { hydrate :: Bool
    , init_uri :: URI
    , init_settings :: JSONSettings
    , init_payload :: InitialDataPayload
    } deriving Eq

type InitCtxRef = IORef AppInitCtx

data MessagesFromChildren = MsgClientMounted
    deriving (Eq, Generic, FromJSON, ToJSON)
