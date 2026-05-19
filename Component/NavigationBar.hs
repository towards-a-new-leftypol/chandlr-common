{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar where

import Miso
    ( Component (bindings)
    , component
    , Effect
    , io_
    , consoleLog
    , View
    , modify
    , vfrag
    , (-->)
    , get
    )
import qualified Data.Set as Set

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.View
import Common.Component.NavigationBar.Model
import Common.Component.NavigationBar.NavMenu
import qualified Common.FrontEnd.Model as FE
import qualified Common.Network.SiteType as Site

app :: Component FE.Model Model Action
app = (component initialModel update view)
  { bindings =
    [ FE.getSetSitesAndBoards --> getSetSitesAndBoards
    ]
  }

initialModel :: Model
initialModel = Model
  { menuState = Closed
  , sitesAndBoards = []
  , currentSites = All
  }

update :: Action -> Effect a Model Action
update ClickSites = do
    io_ $ consoleLog "Choose Sites Clicked!"
    modify $ \m -> m { menuState = ChooseSites }

update ClickBoards = do
    io_ $ consoleLog "Choose Boards Clicked!"
    modify $ \m -> m { menuState = ChooseBoards }

update SubmitMenuChoice = do
    io_ $ consoleLog "Submit Menu Choice!"
    modify $ \m -> m { menuState = Closed }

update CancelMenu = do
    io_ $ consoleLog "CloseMenu!"
    modify $ \m -> m { menuState = Closed }

update (ToggleSite s) = do
    io_ $ consoleLog "toggle site!"
    model <- get
    case currentSites model of
        All -> do
            let
                allSites = Set.fromList $ sitesAndBoards model
                withoutS = Set.delete s allSites

            modify (\m -> m { currentSites = CurrentSites withoutS })

        CurrentSites selectedSites ->
            if Set.member s selectedSites
            then do
                let withoutS = Set.delete s selectedSites
                modify (\m -> m { currentSites = CurrentSites withoutS })
            else do
                let withS = Set.insert s selectedSites
                modify (\m -> m { currentSites = CurrentSites withS })


view :: Model -> View Model Action
view m = vfrag
    [ navmenu m
    , navbar
    ]
