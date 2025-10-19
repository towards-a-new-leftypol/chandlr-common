{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Network.SiteType where

import GHC.Generics
import Miso.String (MisoString)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty
import Data.Maybe (fromJust)
import qualified Common.Network.BoardType as B
import qualified Common.Network.CatalogPostType as Ct
import qualified Common.Network.ThreadType as T
import qualified Common.Network.PostType as P
import qualified Common.AttachmentType as A

data Site = Site
  { site_id :: Int
  , name    :: MisoString
  , url     :: MisoString
  , boards  :: NonEmpty B.Board
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)


emptySite :: Site
emptySite = Site
    { site_id = -1
    , name = ""
    , url = ""
    , boards = singleton B.emptyBoard
    }


fromCatalogPost :: Ct.CatalogPost -> Site
fromCatalogPost p =
    Site
      { site_id = Ct.site_id p
      , name    = Ct.site_name p
      , url     = "Loading..."
      , boards  = singleton board
      }

    where
        board = B.Board
          { B.board_id = -1
          , B.name     = Nothing
          , B.pathpart = Ct.pathpart p
          , B.site_id  = Ct.site_id p
          , B.threads  = singleton thread
          }

        thread = T.Thread
          { T.thread_id       = toInteger $ Ct.thread_id p
          , T.board_thread_id = toInteger $ Ct.board_thread_id p
          , T.creation_time   = Ct.creation_time p
          , T.board_id        = -1
          , T.posts           = singleton post
          }

        post = P.Post
          { P.post_id           = toInteger $ fromJust $ Ct.post_id p
          , P.board_post_id     = toInteger $ Ct.board_post_id p
          , P.creation_time     = Ct.creation_time p
          , P.body              = Ct.body p
          , P.subject           = Ct.subject p
          , P.name              = Ct.name p
          , P.email             = Ct.email p
          , P.body_search_index = ""
          , P.thread_id         = toInteger $ Ct.thread_id p
          , P.embed             = Ct.embed p
          , P.attachments       = maybe [] (: []) attachment
          }

        attachment = do
            mime <- Ct.file_mimetype p
            filename <- Ct.file_name p
            extension <- Ct.file_extension p
            thumb_extension <- Ct.file_thumb_extension p

            return A.Attachment
                { A.mimetype          = mime
                , A.creation_time     = Ct.creation_time p
                , A.sha256_hash       = ""
                , A.phash             = Nothing
                , A.illegal           = False
                , A.post_id           = fromJust $ Ct.post_id p
                , A.resolution        = Ct.file_resolution p
                , A.file_extension    = Just extension
                , A.thumb_extension   = Just thumb_extension
                , A.original_filename = Just filename
                , A.board_filename    = filename
                , A.spoiler           = False
                , A.file_size_bytes   = 0
                , A.attachment_idx    = 1
                }
