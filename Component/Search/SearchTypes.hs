{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.SearchTypes where

import Miso.String (MisoString)
import Miso (Topic, topic)

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

type MessageIn = MisoString
type MessageOut = (MisoString, [ CatalogPost ])

searchInTopic :: Topic MessageIn
searchInTopic = topic "search-in"

searchOutTopic :: Topic MessageOut
searchOutTopic = topic "search-out"
