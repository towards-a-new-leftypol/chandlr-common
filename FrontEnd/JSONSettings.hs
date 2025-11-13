{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.FrontEnd.JSONSettings where

import Miso
    ( View
    )
import Miso.String (MisoString, toMisoString)
import Miso.Html
    ( meta_
    )
import Miso.Html.Property
    ( name_
    , content_
    )
import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSettings = JSONSettings
    { postgrest_url :: MisoString
    , jwt :: String
    , postgrest_fetch_count :: Int
    , media_root :: MisoString
    , media_root_path :: MisoString
    , static_serve_path :: String
    , static_serve_url_root :: String
    , admin :: Bool
    } deriving (Show, Generic)

instance FromJSON JSONSettings


asHtml :: JSONSettings -> [ View model action ]
asHtml settings =
    [ meta "postgrest-url" (toMisoString $ postgrest_url settings)
    , meta "postgrest-fetch-count" (toMisoString $ postgrest_fetch_count settings)
    , meta "media-root" (toMisoString $ media_root settings)
    , meta "admin" (if admin settings then "True" else "False")
    ]

    where
        meta name value = meta_ [ name_ name, content_ value ]
