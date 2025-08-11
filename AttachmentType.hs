{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Common.AttachmentType
( Attachment (..)
, Dimension (..)
, Paths (..)
) where

import GHC.Generics
import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)

data Dimension = Dimension
  { width  :: Int
  , height :: Int
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Paths = Paths
  { file_path :: FilePath
  , thumbnail_path :: Maybe FilePath
  } deriving (Show)

data Attachment = Attachment
    { mimetype          :: MisoString
    , creation_time     :: UTCTime
    , sha256_hash       :: MisoString
    , phash             :: Maybe Int64
    , illegal           :: Bool
    , post_id           :: Int64
    , resolution        :: Maybe Dimension
    , file_extension    :: Maybe MisoString
    , thumb_extension   :: Maybe MisoString
    , original_filename :: Maybe MisoString
    , board_filename    :: MisoString
    , spoiler           :: Bool
    , file_size_bytes   :: Int
    , attachment_idx    :: Int
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
