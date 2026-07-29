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


app :: (Eq context, Eq m, Eq props) => Component context props m a -> Component context props Model Action
app innerComponent = component initialModel update (view innerComponent)


view :: (Eq context, Eq m, Eq props) => Component context props m a -> context -> props -> Model -> View context Action
view innerComponent _ props = const $ vfrag
  [ mountWithProps props innerComponent
  ]

update :: Action -> Effect parent props Model Action
update () = return ()
