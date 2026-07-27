module Common.Component.NavigationBar.Model where

import Miso (URI)
import Data.Set (Set, empty)

import Common.Network.SiteType (Site)
import Common.Network.BoardType (Board)

data Props = Props
    { sitesAndBoards :: [ Site ]
    , currentUri :: URI
    } deriving Eq


data Model = Model
    { menuState :: MenuState
    , currentSites :: CurrentSites
    , selectedBoards :: Set Board
    , allBoardsSelected :: Bool
    , hydrate :: Bool
    } deriving Eq


data MenuState
    = ChooseSites
    | ChooseBoards
    | Closed
    deriving Eq

data CurrentSites = All | CurrentSites (Set Site) deriving Eq

emptyCurrentSites :: CurrentSites
emptyCurrentSites = CurrentSites empty
