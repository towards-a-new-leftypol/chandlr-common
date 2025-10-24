{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.Modal where

import Miso (View, MisoString, text)

import Miso.Html.Element
    ( div_
    , h3_
    , span_
    , button_
    )

import Miso.Html.Property
    ( class_
    )

import Miso.Html.Event
    ( onClick
    )

data Model model a = Model
    { cancel :: a
    , submit :: a
    , content :: View model a
    , title :: MisoString
    , action :: MisoString
    }

view :: Model model a -> View model a
view (Model {..}) =
    div_
        [ class_ "modal-dialog__outermost" ]
        [ div_
            [ class_ "modal-dialog__backdrop"
            , onClick cancel
            ] []
        , div_
            [ class_ "modal-dialog__window" ]
            [ div_
                [ class_ "modal-dialog__window-header" ]
                [ h3_ [ class_ "modal-dialog__title" ] [ text title ]
                , div_
                    [ class_ "modal-dialog__X"
                    , onClick cancel
                    ]
                    [ span_ [] [ "×" ] ]
                ]
            , content
            , div_
                [ class_ "modal-dialog__button-container" ]
                [ button_
                    [ class_ "modal-dialog__button"
                    , class_ "modal-dialog__button--cancel"
                    , onClick cancel
                    ] [ "Cancel" ]
                , button_
                    [ class_ "modal-dialog__button"
                    , class_ "modal-dialog__button--submit"
                    , onClick submit
                    ] [ text action ]
                ]
            ]
        ]
