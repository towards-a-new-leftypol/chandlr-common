{-# LANGUAGE OverloadedStrings #-}

module Common.Component.PostViews where

import Prelude hiding (head)
import Miso
  ( View
  , text
  , Attribute
  , toMisoString
  )
import Miso.Html
  ( div_
  )
import Miso.Html.Property
  ( class_
  , id_
  )
import Data.List.NonEmpty (head)

import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import Common.Network.PostType (Post)
import qualified Common.Network.PostType as Post
import qualified Common.Network.BoardType as Board
import Common.Network.BoardType (Board)
import Common.Network.ThreadType (Thread)
import Common.Component.Thread.Files (files)
import Common.Component.Thread.Intro (intro)
import Common.Component.Thread.Embed (embed)
import Common.Component.Thread.Model
import Common.Parsing.BodyParser
import qualified Common.Component.BodyRender as Body


op
    :: (PostWithBody -> [ View model action ])
    -> Model
    -> Post
    -> Backlinks
    -> [ View model action ]
op introExtras m op_post backlinks =
    [ div_
        (
            class_ (if admin m then "post op post-with-admin" else "post op")
            : id_ (toMisoString $ show $ Post.board_post_id op_post)
            : multi op_post
        )
        ( intro site_ board thread op_post backlinks (current_time m)
        : files_or_embed_view
        : concatMap introExtras (post_bodies m)
        ++
        [ div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
        )
    ]

    where
        files_or_embed_view :: View model a
        files_or_embed_view =
          case Post.embed op_post of
            Just _ -> embed op_post
            Nothing -> files (media_root m) site_ op_post


        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board

        body :: [ PostWithBody ] -> [ View model a ]
        body [] = []
        body (x:_) = Body.render site_ $ snd x


multi :: Post -> [ Attribute a ]
multi post
    | length (Post.attachments post) > 1 = [ class_ "multifile" ]
    | otherwise = []


reply
  :: (PostWithBody -> [ View model action ])
  -> Model
  -> Backlinks
  -> PostWithBody
  -> View model action
reply introExtras m backlinks pwb@(post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        ( class_ (if admin m then "post reply post-with-admin" else "post reply")
        : multi post
        )

        ( intro site_ board thread post backlinks (current_time m)
        : introExtras pwb
        ++
        [ files_or_embed_view
        , div_
            [ class_ "body" ]
            (Body.render site_ parts)
        ]
        )
    ]

    where
        files_or_embed_view :: View model action
        files_or_embed_view =
          case Post.embed post of
            Just _ -> embed post
            Nothing -> files (media_root m) site_ post

        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board
