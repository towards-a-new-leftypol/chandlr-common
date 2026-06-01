module Common.Component.NavigationBar.Model where

import Miso (URI)
import Miso.Lens (Lens, LensCore (..))
import Data.Set (Set, empty)

import Common.Network.SiteType (Site)
import Common.Network.BoardType (Board)

data Model = Model
    { menuState :: MenuState
    , sitesAndBoards :: [ Site ]
    , currentSites :: CurrentSites
    , currentUri :: URI
    , selectedBoards :: Set Board
    } deriving Eq

getSetSitesAndBoards :: Lens Model [ Site ]
getSetSitesAndBoards =
    Lens
        sitesAndBoards
        (\xs model -> model
            { sitesAndBoards = xs
            }
        )

getSetCurrentUri :: Lens Model URI
getSetCurrentUri =
    Lens
        currentUri
        (\x model -> model { currentUri = x })

data MenuState
    = ChooseSites
    | ChooseBoards
    | Closed
    deriving Eq

data CurrentSites = All | CurrentSites (Set Site)  deriving Eq

emptyCurrentSites :: CurrentSites
emptyCurrentSites = CurrentSites empty
