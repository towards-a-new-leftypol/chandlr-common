{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.SiteType where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
import Common.Network.BoardType (Board)

data Site = Site
  { site_id :: Int
  , name    :: Text
  , url     :: Text
  , boards  :: NonEmpty Board
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)
