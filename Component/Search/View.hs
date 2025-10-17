{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.View where

import Miso
  ( View
  )
import Miso.Html.Property
  ( class_
  , action_
  , method_
  , type_
  , value_
  , name_
  )
import Miso.Html
  ( input_
  , form
  , onChange
  , onSubmit
  )

import Common.Component.Search.SearchTypes

view :: Model -> View Model Action
view = const $ form
    [ class_ "search_form"
    , action_ "/search"
    , method_ "GET"
    , onSubmit Submit
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
