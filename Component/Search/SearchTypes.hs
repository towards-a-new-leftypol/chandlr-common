module Common.Component.Search.SearchTypes where

import Data.JSString (JSString)
import Network.Http (HttpResult (..))
import Control.Concurrent.MVar (MVar)

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange JSString
    | OnSubmit
    | ChangeAndSubmit JSString
    | SearchResult (HttpResult [ CatalogPost ])
    | PassPostsToSelf [ CatalogPost ] -- I think I don't understand something about the update type but I had to add this...
    | NoAction

data Model = Model
    { searchTerm :: JSString
    , searchVar :: MVar JSString
    , clientModel :: Client.Model
    , displayResults :: [ CatalogPost ]
    } deriving Eq
