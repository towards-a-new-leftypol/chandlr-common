{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.SearchTypes where

import Miso (Topic, topic, ComponentId, MisoString)
import Miso.Lens (Lens (..))

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | Submit
    | ChangeAndSubmit MisoString
    | SearchResult Client.MessageOut
    | OnMessage MessageIn
    | OnMessageError MisoString
    | Initialize
    deriving Eq

data Model = Model
    { searchTerm :: MisoString
    , intendPushUri :: Bool
    , clientComponentId :: ComponentId
    } deriving Eq

getSetSearchTerm :: Lens Model MisoString
getSetSearchTerm =
    Lens
        searchTerm
        (\s model -> model { searchTerm = s })

getSetClientComponentId :: Lens Model ComponentId
getSetClientComponentId =
    Lens
        clientComponentId
        (\x model -> model { clientComponentId = x })

type MessageIn = (Bool, MisoString) -- the boolean is passed through
type MessageOut = (Bool, MisoString, [ CatalogPost ])

searchInTopic :: Topic MessageIn
searchInTopic = topic "search-in"

searchOutTopic :: Topic MessageOut
searchOutTopic = topic "search-out"
