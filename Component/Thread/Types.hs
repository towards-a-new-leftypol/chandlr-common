{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Common.Component.Thread.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Miso (MisoString, Topic, topic)
import Miso.JSON qualified

import Common.Component.Thread.Model (PostWithBody)
import Common.Network.SiteType (Site)
import Common.MisoAeson


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
    deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving (Miso.JSON.ToJSON, Miso.JSON.FromJSON) via (MisoAeson Message)


threadTopic :: Topic Message
threadTopic = topic "thread"
