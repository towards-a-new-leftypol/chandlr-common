module Common.Component.Search.SearchTypes where

import Miso.String (MisoString)
import Common.Network.HttpTypes (HttpResult (..))
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | ChangeAndSubmit MisoString
    | SearchResult (HttpResult [ CatalogPost ])
    deriving Eq

data Model = Model
    { searchTerm :: MisoString
    , displayResults :: [ CatalogPost ]
    } deriving Eq
