{-# LANGUAGE OverloadedStrings #-}

module Common.Component.InfiniteScroll where

import Prelude hiding ((!!))
import Miso
    ( Component
    , component
    , vfrag
    , Effect
    , mountWithProps
    , View
    , MisoString
    , onCreatedWith
    , consoleLog
    , get
    , io_
    , startSub
    , asyncCallback1
    , toMisoString
    , mailChildren
    )
import Miso.Html.Property (class_)
import Miso.Html
    ( div_
    )
import Miso.DSL
    ( jsg
    , (!!)
    , (!)
    , fromJSVal
    , new
    , (#)
    , setField
    , create
    )
import Control.Monad (void)

import Common.Component.InfiniteScroll.Model
import Common.Component.InfiniteScroll.Action


app
    :: (Eq context, Eq m, Eq props)
    => Component context props m a
    -> MisoString
    -> Component context props Model Action
app innerComponent lbl = component initialModel update (view innerComponent)
    where
        initialModel = Model lbl


view :: (Eq context, Eq m, Eq props) => Component context props m a -> context -> props -> Model -> View context Action
view innerComponent _ props = const $ vfrag
  [ div_
      [ class_ "sentinel sentinel-top"
      , onCreatedWith $ RegisterSentinel Top
      ] []
  , mountWithProps props innerComponent
  , div_
      [ class_ "sentinel sentinel-bottom"
      , onCreatedWith $ RegisterSentinel Bottom
      ] []
  ]

update :: Action -> Effect parent props Model Action
update (RegisterSentinel pos domRef) = do
    model <- get
    let lbl = label model
    io_ $
        consoleLog $ "InfiniteScroll " <> lbl <> " - " <> toMisoString (show pos) <> " Sentinel Registered"

    startSub (lbl <> "-" <> toMisoString (show pos)) $ \sink -> do
        callback <- asyncCallback1 $ \entries -> do
            entry <- entries !! 0
            isIntersecting <- entry ! "isIntersecting" >>= fromJSVal
            case isIntersecting of
                Just True -> sink $ ReachedTarget pos
                _ -> return ()

        options <- create
        setField options "rootMargin" ("750px" :: MisoString)
        setField options "threshold"  (0 :: Double)

        iObsC <- jsg "IntersectionObserver"
        iObs <- new iObsC (callback, options)
        void $ iObs # "observe" $ [ domRef ]

update (ReachedTarget pos) = do
    io_ $ consoleLog $ "InfiniteScroll REACHED " <> toMisoString (show pos)
    mailChildren $ Grow pos
