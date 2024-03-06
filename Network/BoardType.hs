{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.BoardType where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Common.Network.ThreadType (Thread)

data Board = Board
  { board_id :: Int
  , name     :: Maybe Text
  , pathpart :: Text
  , site_id  :: Int
  , threads  :: [ Thread ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

