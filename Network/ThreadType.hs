{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.ThreadType where

import GHC.Generics
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import Common.Network.PostType (Post)

data Thread = Thread
  { thread_id       :: Integer
  , board_thread_id :: Integer
  , creation_time   :: UTCTime
  , board_id        :: Int
  , posts           :: [ Post ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)
