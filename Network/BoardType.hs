{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.BoardType where

import GHC.Generics
import Miso.String (MisoString)
import Miso.JSON (FromJSON, ToJSON)

import Common.Network.ThreadType (Thread)

data Board = Board
  { board_id :: Int
  , name     :: Maybe MisoString
  , pathpart :: MisoString
  , site_id  :: Int
  , threads  :: [ Thread ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

emptyBoard :: Board
emptyBoard = Board
    { board_id = -1
    , name = Nothing
    , pathpart = ""
    , site_id = -1
    , threads = []
    }
