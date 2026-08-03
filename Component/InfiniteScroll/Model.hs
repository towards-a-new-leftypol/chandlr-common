module Common.Component.InfiniteScroll.Model where

import Miso (MisoString)

newtype Model = Model
    { label :: MisoString
    } deriving Eq
