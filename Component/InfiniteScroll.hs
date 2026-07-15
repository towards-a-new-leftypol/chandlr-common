module Common.Component.InfiniteScroll where

import Miso
    ( Component
    , component
    , vfrag
    , Effect
    , mountWithProps
    , View
    )

import Common.Component.InfiniteScroll.Model
import Common.Component.InfiniteScroll.Action

initialModel :: Model
initialModel = ()


app :: (Eq m, Eq props) => Component Model props m a -> Component parent props Model Action
app innerComponent = component initialModel update (view innerComponent)


view :: (Eq m, Eq props) => Component Model props m a -> props -> Model -> View Model Action
view innerComponent props = const $ vfrag
  [ mountWithProps props innerComponent
  ]

update :: Action -> Effect parent props Model Action
update () = return ()
