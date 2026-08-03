module Common.Component.InfiniteScroll.Action where

import Miso.Effect (DOMRef)

data SentinelPosition = Top | Bottom
    deriving Show

data Action
    = RegisterSentinel SentinelPosition DOMRef
    | ReachedTarget SentinelPosition
