{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.PostsType
    ( Post (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON (..), (.=), object)
import Data.Time.Clock (UTCTime)
import Data.Int (Int64)
import Data.Text (Text)

data Post = Post
    { post_id                :: Maybe Int64
    , board_post_id          :: Int64
    , creation_time          :: UTCTime
    , body                   :: Maybe Text
    , name                   :: Maybe Text
    , subject                :: Maybe Text
    , email                  :: Maybe Text
    , thread_id              :: Int64
    , embed                  :: Maybe Text
    , local_idx              :: Int
    , is_missing_attachments :: Bool
    , sage                   :: Bool
    } deriving (Show, Generic, FromJSON)

-- Custom ToJSON instance that excludes the post_id field
instance ToJSON Post where
  toJSON Post {..} =
    object [ "board_post_id" .= board_post_id
           , "creation_time" .= creation_time
           , "body"          .= body
           , "name"          .= name
           , "subject"       .= subject
           , "email"         .= email
           , "thread_id"     .= thread_id
           , "embed"         .= embed
           , "local_idx"     .= local_idx
           ]
