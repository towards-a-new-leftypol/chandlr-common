{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.ThreadType where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
import Data.Time.Clock (UTCTime)

import Common.Network.PostType (Post, emptyPost)
import Common.Utils (fakeTime)

data Thread = Thread
  { thread_id       :: Integer
  , board_thread_id :: Integer
  , creation_time   :: UTCTime
  , board_id        :: Int
  , posts           :: NonEmpty Post
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

emptyThread :: Thread
emptyThread = Thread
    { thread_id = -1
    , board_thread_id = -1
    , creation_time = fakeTime
    , board_id = -1
    , posts = singleton emptyPost
    }
