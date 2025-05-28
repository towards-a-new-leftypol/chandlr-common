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
  , form
  , onChange
  , onSubmit
  )

view :: Model -> View Action
view = const $ form
    [ class_ "search_form"
    , action_ "/search"
    , method_ "GET"
    , onSubmit OnSubmit
    ]
    [ input_
        [ type_ "submit"
        , value_ "🔍"
        ]
    , input_
        [ type_ "text"
        , name_ "search"
        , onChange $ SearchChange
        ]
    ]
