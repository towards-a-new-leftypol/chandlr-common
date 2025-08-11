{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Thread.Embed where

import Miso
  ( View
  , div_
  , class_
  , a_
  , href_
  , target_
  , img_
  , src_
  , span_
  , data_
  )

import Data.Maybe (fromJust)
import Miso.String (MisoString, toMisoString, fromMisoString)
import Data.Either (fromRight)

import qualified Common.Network.PostType as Post
import Common.Network.PostType (Post)
import Common.Parsing.EmbedParser (extractVideoId)

embed :: Post -> View a
embed post = div_
    [ class_ "video-container" ]
    [ a_
        [ href_ $ "https://youtu.be/" <> video_id
        , target_ "_blank"
        , class_ "file"
        ]
        [ img_
            [ data_ "height" "190px"
            , data_ "width" "255px"
            , src_ ("https://leftychan.net/vi/" <> video_id <> "/0.jpg")
            , class_ "post-image"
            ]
        ]
    , span_ [][ "[Embed]" ]
    ]

    where
      video_id :: MisoString
      video_id = toMisoString $ fromJust $
          (Post.embed post) >>= Just . (fromRight "") . extractVideoId . fromMisoString
