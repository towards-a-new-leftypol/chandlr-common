{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings  #-}

module Common.AttachmentType
( Attachment (..)
, Dimension (..)
, Paths (..)
) where

import GHC.Generics
import Miso.JSON
import Data.Int (Int64)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)

import Common.Utils (isoToUtc, utcToIso)

data Dimension = Dimension
  { width  :: Int
  , height :: Int
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

data Paths = Paths
  { file_path :: FilePath
  , thumbnail_path :: Maybe FilePath
  } deriving (Show)

data Attachment = Attachment
    { attachment_id     :: Integer
    , mimetype          :: MisoString
    , creation_time     :: UTCTime
    , sha256_hash       :: MisoString
    , phash             :: Maybe Int64
    , illegal           :: Bool
    , post_id           :: Integer
    , resolution        :: Maybe Dimension
    , file_extension    :: Maybe MisoString
    , thumb_extension   :: Maybe MisoString
    , original_filename :: Maybe MisoString
    , board_filename    :: MisoString
    , spoiler           :: Bool
    , file_size_bytes   :: Int
    , attachment_idx    :: Int
    } deriving (Show, Eq)

instance ToJSON Attachment where
    toJSON (Attachment {..}) =
        object
            [ "attachment_id"     .= toJSON attachment_id
            , "mimetype"          .= toJSON mimetype
            , "creation_time"     .= String (utcToIso creation_time)
            , "sha256_hash"       .= toJSON sha256_hash
            , "phash"             .= encodePhash phash
            , "illegal"           .= toJSON illegal
            , "post_id"           .= Number (fromIntegral post_id)
            , "resolution"        .= toJSON resolution
            , "file_extension"    .= toJSON file_extension
            , "thumb_extension"   .= toJSON thumb_extension
            , "original_filename" .= toJSON original_filename
            , "board_filename"    .= toJSON board_filename
            , "spoiler"           .= toJSON spoiler
            , "file_size_bytes"   .= toJSON file_size_bytes
            , "attachment_idx"    .= toJSON attachment_idx
            ]

        where
            encodePhash :: Maybe Int64 -> Value
            encodePhash Nothing = Null
            encodePhash (Just i)  = Number (fromIntegral i)

instance FromJSON Attachment where
    parseJSON (Object a) =
        Attachment
            <$> a .: "attachment_id"
            <*> a .: "mimetype"
            <*> (a .: "creation_time" >>= isoToUtc)
            <*> a .: "sha256_hash"
            <*> a .: "phash"
            <*> a .: "illegal"
            <*> a .: "post_id"
            <*> a .: "resolution"
            <*> a .: "file_extension"
            <*> a .: "thumb_extension"
            <*> a .: "original_filename"
            <*> a .: "board_filename"
            <*> a .: "spoiler"
            <*> a .: "file_size_bytes"
            <*> a .: "attachment_idx"

    parseJSON _ = fail "Expected Object for Attachment"
