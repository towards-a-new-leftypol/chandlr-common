module Common.Component.Thread.Model where

import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso.Lens (Lens (..))

import Common.Network.SiteType (Site)
import Common.Network.PostType (Post)
import Common.Parsing.PostPartType (PostPart)

type PostWithBody = (Post, [ PostPart ])

data Model
  = Uninitialized
  | Model
    { site :: Site
    , media_root :: MisoString
    , post_bodies :: [ PostWithBody ]
    , current_time :: UTCTime
    , admin :: Bool
    } deriving Eq


getSetAdmin :: Lens Model Bool
getSetAdmin =
    Lens
        admin
        (\s model -> model { admin = s })
