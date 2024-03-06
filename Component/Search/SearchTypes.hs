module Common.Component.Search.SearchTypes where

import Miso.String (MisoString)
import Common.Network.HttpTypes (HttpResult (..))
import Control.Concurrent.MVar (MVar)

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | ChangeAndSubmit MisoString
    | SearchResult (HttpResult [ CatalogPost ])
    | PassPostsToSelf [ CatalogPost ] -- I think I don't understand something about the update type but I had to add this...
    | NoAction

data Model = Model
    { searchTerm :: MisoString
    , searchVar :: MVar MisoString
    , clientModel :: Client.Model
    , displayResults :: [ CatalogPost ]
    } deriving Eq
