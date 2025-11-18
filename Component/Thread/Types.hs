{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.Thread.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Miso (MisoString, Topic, topic)


import Common.Component.Thread.Model (PostWithBody)
import Common.Network.SiteType (Site)

data Action
    = OnMessage Message
    | OnMessageError MisoString
    | UpdatePostBodies UTCTime [ PostWithBody ]
    | Initialize
    | OnDeleteBtn PostWithBody
    deriving Eq


data Message
    = RenderSite MisoString Site
    | PostDeleted [ Integer ]
    deriving (Eq, Generic, ToJSON, FromJSON)


threadTopic :: Topic Message
threadTopic = topic "thread"
