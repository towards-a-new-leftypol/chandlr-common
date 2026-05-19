module Common.Component.NavigationBar.Model where

import Miso.Lens (Lens, LensCore (..))
import Data.Set (Set)

import Common.Network.SiteType (Site)

data Model = Model
    { menuState :: MenuState
    , sitesAndBoards :: [ Site ]
    , currentSites :: CurrentSites
    } deriving Eq

getSetSitesAndBoards :: Lens Model [ Site ]
getSetSitesAndBoards =
    Lens
        sitesAndBoards
        (\xs model -> model { sitesAndBoards = xs })

data MenuState
    = ChooseSites
    | ChooseBoards
    | Closed
    deriving Eq

data CurrentSites = All | CurrentSites (Set Site)  deriving Eq
