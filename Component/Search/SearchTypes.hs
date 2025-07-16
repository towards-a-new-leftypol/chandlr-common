{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.SearchTypes where

import Data.Aeson (Result)
import Miso.String (MisoString)
import Miso (Topic, topic)

import qualified Common.Network.ClientTypes as Client
import Common.Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange MisoString
    | OnSubmit
    | ChangeAndSubmit MisoString
    | SearchResult (Result Client.MessageOut)
    | OnMessage (Result Message)
    | Initialize
    deriving Eq

data Model = Model
    { searchTerm :: MisoString
    , displayResults :: [ CatalogPost ]
    } deriving Eq

type Message = MisoString

searchTopic :: Topic Message
searchTopic = topic "search"
