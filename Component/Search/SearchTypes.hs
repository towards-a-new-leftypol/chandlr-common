{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.SearchTypes where

import Miso.String (MisoString)
import Miso (Topic, topic)
import Miso.Lens (Lens (..))

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | ChangeAndSubmit MisoString
    | SearchResult Client.MessageOut
    | OnMessage MessageIn
    | OnMessageError MisoString
    | Initialize
    deriving Eq

newtype Model = Model
    { searchTerm :: MisoString
    } deriving Eq

getSetSearchTerm :: Lens Model MisoString
getSetSearchTerm =
    Lens
        searchTerm
        (\s model -> model { searchTerm = s })

type MessageIn = MisoString
type MessageOut = (MisoString, [ CatalogPost ])

searchInTopic :: Topic MessageIn
searchInTopic = topic "search-in"

searchOutTopic :: Topic MessageOut
searchOutTopic = topic "search-out"
