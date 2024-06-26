module Common.Server.JSONSettings
  ( JSONSettings(..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSettings = JSONSettings
  { postgrest_url :: String
  , jwt :: Text
  , backup_read_root :: FilePath
  , media_root_path :: FilePath
  , site_name :: String
  , site_url :: String
  } deriving (Show, Generic)

instance FromJSON JSONSettings
