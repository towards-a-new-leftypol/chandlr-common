{-# LANGUAGE OverloadedStrings #-}

module Common.Admin.DeleteBtn where

import Miso (View)
import Miso.Html
    ( span_
    )
import Miso.Html.Property
    ( class_
    )
import Miso.Html.Event
    ( onClick
    )

deleteBtn :: action -> View model action
deleteBtn a = span_
    [ class_ "post-illegal-delete-btn"
    , onClick a
    ]
    [ "delete" ]
