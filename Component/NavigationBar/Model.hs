module Common.Component.NavigationBar.Model where

import Miso (URI)
import Miso.Lens (Lens, LensCore (..))
import Data.Set (Set, empty)
import qualified Data.Set as Set

import Common.Network.SiteType (Site, site_id)
import Common.Network.BoardType (Board)

data Model = Model
    { menuState :: MenuState
    , sitesAndBoards :: [ Site ]
    , currentSites :: CurrentSites
    , currentUri :: URI
    , selectedBoards :: Set Board
    , allBoardsSelected :: Bool
    } deriving Eq

getSetSitesAndBoards :: Lens Model [ Site ]
getSetSitesAndBoards =
    Lens
        sitesAndBoards
        (\xs model ->
            let cs = case currentSites model of
                    All -> All
                    CurrentSites oldSet ->
                        let oldIds = Set.map site_id oldSet
                        in CurrentSites $ Set.fromList
                            [ s
                            | s <- xs
                            , site_id s `Set.member` oldIds
                            ]
            in  model
                    { sitesAndBoards = xs
                    , currentSites = cs
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
