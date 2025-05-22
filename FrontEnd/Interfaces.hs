module Common.FrontEnd.Interfaces where

import Data.Aeson (FromJSON)

import Common.FrontEnd.Action
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.ThreadView as Thread
import qualified Common.Network.ClientTypes as Client
import qualified Common.Network.HttpTypes as Client
import qualified Common.Component.CatalogGrid as Grid

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
