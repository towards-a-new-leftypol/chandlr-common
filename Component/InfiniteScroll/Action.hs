{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.InfiniteScroll.Action where

import Miso.Effect (DOMRef)
import GHC.Generics (Generic)
import Miso.JSON (ToJSON, FromJSON)

data SentinelPosition = Top | Bottom
    deriving (Show, Generic, ToJSON, FromJSON)

data Action
    = RegisterSentinel SentinelPosition DOMRef
    | ReachedTarget SentinelPosition

data InfScrollOutMsg
    = Grow SentinelPosition
    | Trim SentinelPosition
    | Jump SentinelPosition
    deriving (Generic, ToJSON, FromJSON)

data InfScrollInMsg
    = Loaded SentinelPosition
    | Exhausted SentinelPosition
    deriving (Generic, ToJSON, FromJSON)
