{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.BoardType where

import GHC.Generics
import Miso.String (MisoString)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty

import Common.Network.ThreadType (Thread, emptyThread)

data Board = Board
  { board_id :: Int
  , name     :: Maybe MisoString
  , pathpart :: MisoString
  , site_id  :: Int
  , threads  :: NonEmpty Thread
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

emptyBoard :: Board
emptyBoard = Board
    { board_id = -1
    , name = Nothing
    , pathpart = ""
    , site_id = -1
    , threads = singleton emptyThread
    }
