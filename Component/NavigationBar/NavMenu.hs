{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.NavMenu where

import Miso (View, MisoString, text, vfrag)
import Miso.Html hiding (style_)
import Miso.Html.Property

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Component.Modal as Modal

navmenu :: Model -> View Model Action
navmenu (Model Closed) = vfrag []
navmenu m = div_ [ class_ "modal-dialog" ]
    [ Modal.view
        Modal.Model
            { Modal.cancel = CancelMenu
            , Modal.submit = SubmitMenuChoice
            , Modal.content = content m
            , Modal.title = title m
            , Modal.action = "Apply"
            }
    ]

    where
        content :: Model -> View Model Action
        content m_ = h1_ [] [ text $ title m_ ]
        -- content (Model ChooseSites) = h1_ [] [ "Choose Sites" ]
        -- content (Model ChooseBoards) = h1_ [] [ "Choose Boards" ]
        -- content (Model Closed) = undefined

        title :: Model -> MisoString
        title (Model ChooseSites) = "Choose Sites"
        title (Model ChooseBoards) = "Choose Boards"
        title _ = ""
