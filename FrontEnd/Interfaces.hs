module Common.FrontEnd.Interfaces where

import Data.Aeson (FromJSON)

import Common.FrontEnd.Action
import qualified Common.Component.Search as Search
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.ThreadView as Thread
import qualified Common.Network.ClientTypes as Client
import qualified Common.Network.HttpTypes as Client
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.CatalogGrid as Grid

iGrid :: Grid.Interface Action
iGrid = Grid.Interface
    { Grid.passAction = GridAction
    , Grid.threadSelected = mkGetThread
    }

    where
        mkGetThread :: CatalogPost -> Action
        mkGetThread post = GetThread GetThreadArgs
            { website = CatalogPost.site_name post
            , board_pathpart = CatalogPost.pathpart post
            , board_thread_id = CatalogPost.board_thread_id post
            }

iClient :: (FromJSON a) => (Client.HttpResult a -> Action) -> Client.Interface Action a
iClient action = Client.Interface
    { Client.passAction = ClientAction action
    , Client.returnResult = action
    }

iThread :: Thread.Interface Action
iThread = Thread.Interface { Thread.passAction = ThreadAction }

iTime :: TC.Interface Action
iTime = TC.Interface
  { TC.passAction = TimeAction
  , TC.goTo = GoToTime
  }

iSearch :: Search.Interface Action
iSearch =
  Search.Interface
    { Search.passAction = SearchAction
    , Search.clientIface = iClient $ SearchAction . Search.SearchResult
    , Search.searchResults = SearchResults
    }
