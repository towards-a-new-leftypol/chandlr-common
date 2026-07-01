{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module Common.Component.NavigationBar where

import Miso
    ( Component
    , component
    , Effect
    , io_
    , io
    , View
    , modify
    , vfrag
    , get
    , URI
    , emptyURI
    , Topic
    , topic
    , get
    , publish
    , MisoString
    , fromMisoString
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
#ifdef FRONT_END
import JSFFI.MisoFFI (deleteCookie, setCookie, getCookie)
import Common.BitField
import qualified Common.Network.SiteType as Site
import Data.List.NonEmpty (toList)
import Control.Monad (void)
import Miso
    ( Component (bindings, mount)
    , consoleLog
    , (-->)
    , issue
    , toMisoString
    )
#endif

boardsSelCookieName :: MisoString
boardsSelCookieName = "b"

initialModel :: Model
initialModel = Model
  { menuState = Closed
  , sitesAndBoards = []
  , currentSites = CurrentSites Set.empty
  , currentUri = emptyURI
  , selectedBoards = Set.empty
  , allBoardsSelected = True
  }

app :: Component FE.Model Model Action
#ifndef FRONT_END
app = component initialModel undefined view
#else
app = (component initialModel update view)
    { bindings =
        [ FE.getSetSitesAndBoards --> getSetSitesAndBoards
        , FE.getSetCurrentUri --> getSetCurrentUri
        ]
    , mount = Just Initialize
    }

update :: Action -> Effect a Model Action
update Initialize = do
    io_ $ consoleLog "NavigationBar Initialize"
    model <- get
    io $ do
        consoleLog $ "NavigationBar has " <> toMisoString (show $ length $ sitesAndBoards model) <> " sites."
        bCookie <- getCookie boardsSelCookieName

        case bCookie of
            Nothing -> do
                consoleLog $ "NavigationBar didn't find a b cookie"
                return Noop
            Just b -> do
                consoleLog $ "NavigationBar b cookie value: " <> b
                return $ InitSelectedSites $ getBoardIdsFromMisoString b

    where
        getBoardIdsFromMisoString :: MisoString -> Set.Set Int
        getBoardIdsFromMisoString = intsFromBitField . read . fromMisoString

update Noop = return ()

update (InitSelectedSites boardIds) =
    modify $ \m ->
        let
            (sites, boards) =
                foldr step (Set.empty, Set.empty) (sitesAndBoards m)
        in
            m
                { currentSites = CurrentSites sites
                , selectedBoards = boards
                }

    where
        step s (ss, bs) =
            let ms = filter
                    ((`Set.member` boardIds) . Board.board_id)
                    (toList $ Site.boards s)
            in if null ms
                then (ss, bs)
                else
                    ( Set.insert s ss
                    , bs `Set.union` Set.fromList ms
                    )

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

            modify $ \m -> m
                { currentSites = CurrentSites withoutS
                , allBoardsSelected = False
                }
            issue $ RemoveFromSite s

        CurrentSites selectedSites ->
            if Set.member s selectedSites
            then do
                let withoutS = Set.delete s selectedSites
                modify $ \m -> m
                    { currentSites = CurrentSites withoutS
                    , allBoardsSelected = False
                    }
                issue $ RemoveFromSite s
            else do
                let withS = Set.insert s selectedSites
                modify $ \m -> m
                    { currentSites = CurrentSites withS
                    , allBoardsSelected = False
                    }
                issue $ AddFromSite s

update (ToggleBoard b) = do
    io_ $ consoleLog $ "toggle board" <> Board.pathpart b

    modify $ \m ->
        if Set.member b (selectedBoards m)
        then
            m
                { selectedBoards = Set.delete b (selectedBoards m)
                , allBoardsSelected = False
                }
        else
            m
                { selectedBoards = Set.insert b (selectedBoards m)
                , allBoardsSelected = False -- this is kinda lazy, we should check if all the boards are actually selected maybe
                }

    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update (AddFromSite s) = do
    io_ $ consoleLog $ "AddFromSite " <> Site.name s
    modify $ \m -> m
        { selectedBoards = selectedBoards m
            `Set.union` Set.fromList (toList $ Site.boards s)
        , allBoardsSelected = False
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update (RemoveFromSite s) = do
    io_ $ consoleLog $ "RemoveFromSite " <> Site.name s
    modify $ \m -> m
        { selectedBoards = selectedBoards m
            `Set.difference` Set.fromList (toList $ Site.boards s)
        , allBoardsSelected = False
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update SelectAllSites = do
    modify $ \m -> m
        { currentSites = CurrentSites (Set.fromList $ sitesAndBoards m)
        , selectedBoards = Set.fromList $
            concatMap (toList . Site.boards) $ sitesAndBoards m
        , allBoardsSelected = True
        }
    issue ReloadCatalogGridBecauseSelectedBoardsChanged

update SelectNoSites = do
  modify $ \m -> m
      { currentSites = emptyCurrentSites
      , selectedBoards = Set.empty
      }
  issue ReloadCatalogGridBecauseSelectedBoardsChanged

update ReloadCatalogGridBecauseSelectedBoardsChanged = do
    model <- get

    io_ $ do
        consoleLog "ReloadCatalogGridBecauseSelectedBoardsChanged"
        publish navigationBarTopic
            (SelectedBoardsChanged $ Set.toList $ selectedBoards model)

        if allBoardsSelected model
        then
            void $ deleteCookie boardsSelCookieName
        else
            let cookieval = toMisoString $ show $
                    bitFieldFromInts $ Set.map Board.board_id $ selectedBoards model
            in void $ setCookie boardsSelCookieName cookieval
#endif


view :: Model -> View Model Action
view m = vfrag
    [ navmenu m
    , navbar m
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


data OutMessage
    = GoToCatalog
    | SelectedBoardsChanged [ Board.Board ]
    deriving (Generic, FromJSON, ToJSON)

navigationBarTopic :: Topic OutMessage
navigationBarTopic = topic "navigation-bar"
