{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.CatalogPostType
    ( CatalogPost (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone
import Miso.String (MisoString)
import Common.AttachmentType (Dimension)

data CatalogPost = CatalogPost
    { post_id              :: Maybe Integer
    , board_post_id        :: Integer
    , board_thread_id      :: Integer
    , creation_time        :: UTCTime
    , bump_time            :: UTCTime
    , body                 :: Maybe MisoString
    , name                 :: Maybe MisoString
    , subject              :: Maybe MisoString
    , email                :: Maybe MisoString
    , thread_id            :: Int
    -- , post_count           :: Int
    , embed                :: Maybe MisoString
    , estimated_post_count :: Int
    , site_name            :: MisoString
    , pathpart             :: MisoString
    , site_id              :: Int
    , file_mimetype        :: Maybe MisoString
    , file_illegal         :: Maybe Bool
    , file_resolution      :: Maybe Dimension
    , file_name            :: Maybe MisoString
    , file_extension       :: Maybe MisoString
    , file_thumb_extension :: Maybe MisoString
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
