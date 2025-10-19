{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.Thread.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)

import Common.Component.Thread.Model (PostWithBody)
import Common.Network.SiteType (Site)

data Action
    = OnMessage Message
    | OnMessageError MisoString
    | UpdatePostBodies UTCTime [ PostWithBody ]
    | Initialize
    | OnDeleteBtn
    deriving Eq

data Message = RenderSite MisoString Site
    deriving (Eq, Generic, ToJSON, FromJSON)

