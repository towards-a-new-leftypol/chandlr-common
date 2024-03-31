module Common.Server.ConsumerSettings where

import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSiteSettings = JSONSiteSettings
    { name :: String
    , root_url :: String
    } deriving (Show, Generic)

instance FromJSON JSONSiteSettings

data ConsumerJSONSettings = ConsumerJSONSettings
    { websites :: JSONSiteSettings
    } deriving (Show, Generic)

instance FromJSON ConsumerJSONSettings
