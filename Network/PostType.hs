{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.PostType where

import GHC.Generics
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import Common.AttachmentType (Attachment)

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

