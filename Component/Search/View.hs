{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.View where

import Common.Component.Search.SearchTypes

import Miso
  ( View
  , class_
  , action_
  , method_
  , input_
  , type_
  , value_
  , name_
  , form_
  , onChange
  , onSubmit
  )

view :: Interface a -> Model -> View a
view iface _ = form_
    [ class_ "search_form"
    , action_ "/search"
    , method_ "GET"
    , onSubmit $ pass $ OnSubmit
    ]
    [ input_
        [ type_ "submit"
        , value_ "🔍"
        ]
    , input_
        [ type_ "text"
        , name_ "search"
        , onChange $ pass . SearchChange
        ]
    ]

    where
        pass = passAction iface
