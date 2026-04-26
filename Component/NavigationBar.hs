{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar where

import Miso
    ( Component
    , component
    , Effect
    , io_
    , consoleLog
    , View
    , modify
    )
import Miso.Html (div_)

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.View
import Common.Component.NavigationBar.Model
import Common.Component.NavigationBar.NavMenu

app :: Component parent Model Action
app = component False update view

update :: Action -> Effect a Model Action
update ClickSites = do
    io_ $ consoleLog "Select Sites Clicked!"
    modify $ const True

update SubmitMenuChoice = do
    io_ $ consoleLog "Submit Menu Choice!"
    modify $ const False

update CancelMenu = do
    io_ $ consoleLog "CloseMenu!"
    modify $ const False

view :: Model -> View Model Action
view m = div_ []
    [ navbar
    , navmenu m
    ]
