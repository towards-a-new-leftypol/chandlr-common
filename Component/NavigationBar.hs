{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    , URI
    , emptyURI
    , Topic
    , topic
    , get
    , publish
    )

import Miso.JSON (FromJSON, ToJSON)
import qualified Data.Set as Set
import GHC.Generics

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.View
import Common.Component.NavigationBar.Model
import Common.Component.NavigationBar.NavMenu
import qualified Common.FrontEnd.Model as FE
import qualified Common.Network.BoardType as Board
import Common.Utils

app :: Component FE.Model Model Action
app = (component initialModel update view)
  { bindings =
    [ FE.getSetSitesAndBoards --> getSetSitesAndBoards
    , FE.getSetCurrentUri --> getSetCurrentUri
    ]
  }

initialModel :: Model
initialModel = Model
  { menuState = Closed
  , sitesAndBoards = []
  , currentSites = All
  , currentUri = emptyURI
  }

update :: Action -> Effect a Model Action
update ClickSites = do
    io_ $ consoleLog "Choose Sites Clicked!"
    changeMenuStateOrNavigate ChooseSites

update ClickBoards = do
    io_ $ consoleLog "Choose Boards Clicked!"
    changeMenuStateOrNavigate ChooseBoards

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

update (ToggleBoard b) = do
    io_ $ consoleLog $ "toggle board" <> Board.pathpart b

update SelectAllSites =
  modify (\m -> m { currentSites = CurrentSites (Set.fromList $ sitesAndBoards m) })

update SelectNoSites =
  modify (\m -> m { currentSites = emptyCurrentSites })

view :: Model -> View Model Action
view m = vfrag
    [ navmenu m
    , navbar
    ]

shouldNavigateBackToCatalog :: URI -> Bool
shouldNavigateBackToCatalog u
    | pageTypeFromURI u == Catalog = False
    | otherwise                    = True


changeMenuStateOrNavigate :: MenuState -> Effect a Model Action
changeMenuStateOrNavigate newstate = do
    model <- get
    if shouldNavigateBackToCatalog (currentUri model)
    then
        io_ $ publish navigationBarTopic GoToCatalog
    else
        modify $ \m -> m { menuState = newstate }


data OutMessage = GoToCatalog
    deriving (Generic, FromJSON, ToJSON)

navigationBarTopic :: Topic OutMessage
navigationBarTopic = topic "navigation-bar"
