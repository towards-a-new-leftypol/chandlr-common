{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.Thread.Files where

import Miso
  ( View
  , div_
  , href_
  , a_
  , class_
  , title_
  , alt_
  , src_
  , style_
  , img_
  , span_
  , loading_
  , download_
  , p_
  , Attribute
  , text
  , target_
  )

import Data.Foldable (toList)
import qualified Data.Text as Text
import Miso.String (append, toMisoString)
import qualified Data.Map as Map
import GHCJS.DOM.Types (JSString)
import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType as Post
import Common.Network.PostType (Post)
import Common.AttachmentType (Attachment, Dimension (..))
import qualified Common.AttachmentType as Attachment
import Network.Units (bytesToHumanReadable)

max_thumbnail_width :: Int
max_thumbnail_width = 255

max_thumbnail_height :: Int
max_thumbnail_height = 255

max_original_filename_display_length :: Int
max_original_filename_display_length = 25

files :: JSString -> Site -> Post -> View a
files media_root site post = div_
  [ class_ "files" ]
  ( map (file media_root site multi) as )

  where
    multi = length as > 1

    as = Post.attachments post

file :: JSString -> Site -> Bool -> Attachment -> View a
file media_root site multifile a = div_
  ( [ class_ "file" ] ++
    if multifile then
      [ class_ "multifile" ] ++ file_elem_size_attr
    else []
  )
  [ p_
      [ class_ "fileinfo" ]
      [ span_ [] [ "File: " ]
      , a_
          [ href_ file_url
          ][ text $ toMisoString board_filename]
      , text " "
      , span_
          [ class_ "details" ]
          [ text $ "(" `append` size `append` res_str
          , a_
              [ download_ orig_file_name
              , href_ file_url
              , title_ $ "Save as original filename (" `append` orig_file_name `append` ")"
              ][ text filename_text ]
          , ")"
          ]
      ]
  , a_
      [ href_ file_url
      , target_ "blank_"
      ]
      [ img_
          (
            [ class_ "post-image"
            , loading_ "lazy"
            , src_ thumb_url
            , alt_ ""
            ] ++ size_style_attr
          )
      ]
  ]

  where
    orig_file_name :: JSString
    orig_file_name = toMisoString fname

    size :: JSString
    size = toMisoString $
      bytesToHumanReadable (Attachment.file_size_bytes a) True

    res_str :: JSString
    res_str = maybe "" show_dimension $ Attachment.resolution a

    show_dimension :: Attachment.Dimension -> JSString
    show_dimension Attachment.Dimension {..} = toMisoString $
        ", " ++ show width ++ "x" ++ show height ++ ", "

    filename_text :: JSString
    filename_text
      | Text.length fname > max_original_filename_display_length =
          toMisoString (Text.take max_original_filename_display_length fname)
          `append` "…" `append` toMisoString file_ext
      | otherwise = toMisoString fname

    fname :: Text.Text
    fname = maybe board_filename id $ Attachment.original_filename a

    file_ext :: Text.Text
    file_ext = maybe "" ((<>) ".") $ Attachment.file_extension a

    board_filename :: Text.Text
    board_filename = Attachment.board_filename a <> file_ext

    thumb_url :: JSString
    thumb_url = img_url_path
      `append` "/thumbnail_" `append` toMisoString (Attachment.board_filename a)
      `append` toMisoString  (maybe "" ((<>) ".") $ Attachment.thumb_extension a)

    file_url :: JSString
    file_url = img_url_path
      `append` "/" `append` toMisoString (Attachment.board_filename a)
      `append` toMisoString  file_ext

    img_url_path :: JSString
    img_url_path
      = media_root
      `append` "/" `append` toMisoString (Site.name site)
      `append` "/" `append` toMisoString (Board.pathpart board)
      `append` "/" `append` toMisoString
        (show $ Thread.board_thread_id
            (head (Board.threads board)))

    board :: Board.Board
    board = head $ Site.boards site

    size_style_attr :: [ Attribute a ]
    size_style_attr = map (mk_size_style_attr . thumb_dimensions) $ toList $ Attachment.resolution a

    file_elem_size_attr :: [ Attribute a ]
    file_elem_size_attr = map (mk_file_elem_width_style . thumb_dimensions) $ toList $ Attachment.resolution a

    mk_file_elem_width_style :: Dimension -> Attribute a
    mk_file_elem_width_style Dimension {..} =
      style_ $ Map.singleton "width" $ toPx (width + 40)

    mk_size_style_attr :: Dimension -> Attribute a
    mk_size_style_attr Dimension {..} = style_ $ Map.fromList
      [
          ( "width"
          , toPx width
          )
        ,
          ( "height"
          , toPx height
          )
      ]

    toPx :: Int -> JSString
    toPx i = (toMisoString $ show i) `append` "px"

    thumb_dimensions :: Dimension -> Dimension
    thumb_dimensions Dimension {..}
      | width > height = Dimension mw (round $ fromIntegral mw / fromIntegral width * (fromIntegral height :: Double))
      | otherwise = Dimension (round $ fromIntegral mh / fromIntegral height * (fromIntegral width :: Double)) mh
      where
          mw = min max_thumbnail_width width
          mh = min max_thumbnail_height height

