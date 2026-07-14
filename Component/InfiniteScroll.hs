module Common.Component.InfiniteScroll where

import Miso
    ( Component
    , component
    , vfrag
    , Effect
    , mount_
    )

import Common.Component.InfiniteScroll.Model
import Common.Component.InfiniteScroll.Action

initialModel :: Model
initialModel = ()



app :: (Eq m) => Component Model m a -> Component parent Model Action
app innerComponent = component initialModel update
  (const $ vfrag
  [ mount_ innerComponent
  ])

update :: Action -> Effect parent Model Action
update () = return ()
