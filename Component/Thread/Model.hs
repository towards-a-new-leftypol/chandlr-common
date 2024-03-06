module Common.Component.Thread.Model where

import Data.Time.Clock (UTCTime)
import GHCJS.DOM.Types (JSString)
import Common.Network.SiteType (Site)
import Common.Network.PostType (Post)
import Common.Parsing.PostPartType (PostPart)

type PostWithBody = (Post, [ PostPart ])

data Model = Model
  { site :: Site
  , media_root :: JSString
  , post_bodies :: [ PostWithBody ]
  , current_time :: UTCTime
  } deriving Eq

