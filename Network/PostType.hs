{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.PostType where

import GHC.Generics
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import Common.AttachmentType (Attachment)
import Common.Utils (fakeTime)

data Post = Post
  { post_id           :: Integer
  , board_post_id     :: Integer
  , creation_time     :: UTCTime
  , body              :: Maybe MisoString
  , subject           :: Maybe MisoString
  , name              :: Maybe MisoString
  , email             :: Maybe MisoString
  , body_search_index :: MisoString
  , thread_id         :: Integer
  , embed             :: Maybe MisoString
  , attachments       :: [ Attachment ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)


emptyPost :: Post
emptyPost = Post
  { post_id = -1
  , board_post_id = -1
  , creation_time = fakeTime
  , body = Nothing
  , subject = Nothing
  , name = Nothing
  , email = Nothing
  , body_search_index = ""
  , thread_id = -1
  , embed = Nothing
  , attachments = []
  }
