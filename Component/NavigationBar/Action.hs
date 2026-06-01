module Common.Component.NavigationBar.Action where

import Common.Network.SiteType (Site)
import Common.Network.BoardType (Board)

data Action
    = ClickSites
    | ClickBoards
    | CancelMenu
    | SubmitMenuChoice
    | ToggleSite Site
    | ToggleBoard Board
    | SelectAllSites
    | SelectNoSites
    | AddFromSite Site
    | RemoveFromSite Site
