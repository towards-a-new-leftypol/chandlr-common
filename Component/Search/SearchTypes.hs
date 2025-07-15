module Common.Component.Search.SearchTypes where

import Data.Aeson (Result)
import Miso.String (MisoString)

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | ChangeAndSubmit MisoString
    | SearchResult (Result Client.MessageOut)
    | Initialize
    deriving Eq

data Model = Model
    { searchTerm :: MisoString
    , displayResults :: [ CatalogPost ]
    } deriving Eq
