module Common.Server.ConsumerSettings where

import Data.Text (Text)
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
    , jwt :: Text
    , media_root_path :: String
    , http_fill_all :: Bool
    , http_sync_continously :: Bool
    } deriving (Show, Generic)

instance FromJSON ConsumerJSONSettings
