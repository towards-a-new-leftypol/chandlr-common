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
app = component initialModel update view

initialModel :: Model
initialModel = Model Closed

update :: Action -> Effect a Model Action
update ClickSites = do
    io_ $ consoleLog "Choose Sites Clicked!"
    modify $ const $ Model ChooseSites

update ClickBoards = do
    io_ $ consoleLog "Choose Boards Clicked!"
    modify $ const $ Model ChooseBoards

update SubmitMenuChoice = do
    io_ $ consoleLog "Submit Menu Choice!"
    modify $ const $ Model Closed

update CancelMenu = do
    io_ $ consoleLog "CloseMenu!"
    modify $ const $ Model Closed

view :: Model -> View Model Action
view m = div_ []
    [ navbar
    , navmenu m
    ]
