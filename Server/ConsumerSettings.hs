module Common.Server.ConsumerSettings where

import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSiteSettings = JSONSiteSettings
    { name :: String
    , root_url :: String
    , boards :: [ String ]
    } deriving (Show, Generic)

instance FromJSON JSONSiteSettings

data ConsumerJSONSettings = ConsumerJSONSettings
    { websites :: [ JSONSiteSettings ]
    , postgrest_url :: String
    , jwt :: String
    , media_root_path :: String
    } deriving (Show, Generic)

instance FromJSON ConsumerJSONSettings
