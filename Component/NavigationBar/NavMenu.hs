{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.NavMenu where

import Miso (View)
import Miso.Html hiding (style_)
import Miso.Html.Property
import Miso.CSS
    ( style_
    , display
    )

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Component.Modal as Modal

navmenu :: Model -> View Model Action
navmenu False = div_ [ style_ [ display "none" ] ] []
navmenu True = div_ [ class_ "modal-dialog" ]
    [ Modal.view
        Modal.Model
            { Modal.cancel = CancelMenu
            , Modal.submit = SubmitMenuChoice
            , Modal.content = h1_ [] [ "Hello NavMenu" ]
            , Modal.title = "Asdf asdf asdf"
            , Modal.action = "Apply"
            }
    ]
