{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.JSONSettings where

import Miso (View)
import Miso.String (toMisoString)
import Miso.Html
    ( meta_
    )
import Miso.Html.Property
    ( name_
    , content_
    )
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data JSONSettings = JSONSettings
    { postgrest_url :: String
    , jwt :: String
    , postgrest_fetch_count :: Int
    , media_root :: String
    , static_serve_path :: String
    , static_serve_url_root :: String
    } deriving (Show, Generic)

instance FromJSON JSONSettings
instance ToJSON JSONSettings

asHtml :: JSONSettings -> [ View model action ]
asHtml settings =
    [ meta "postgrest-root" (toMisoString $ postgrest_url settings)
    , meta "postgrest-fetch-count" (toMisoString $ postgrest_fetch_count settings)
    , meta "media-root" (toMisoString $ media_root settings)
    ]

    where
        meta name value = meta_ [ name_ name, content_ value ]
