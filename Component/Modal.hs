{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.Modal where

import Miso (View)

import Miso.Html.Element
    ( div_
    )

import Miso.Html.Property
    ( class_
    )

data Model model a = Model
    { cancel :: a
    , content :: View model a
    }

view :: Model model a -> View model a
view (Model {..}) =
    div_
        [ class_ "modal-dialog__outermost" ]
        [ div_
            [ class_ "modal-dialog__content-wrapper" ]
            [ content ]
        ]
