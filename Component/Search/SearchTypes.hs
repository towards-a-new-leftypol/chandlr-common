{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Search.SearchTypes where

import Miso.String (MisoString)
import Miso (Topic, topic)
import Miso.Lens (Lens, LensCore (..))
import Miso.JSON

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
    | OnUnmount
    deriving Eq

data Model = Model
    { searchTerm :: MisoString
    , intendPushUri :: Bool
    } deriving Eq

getSetSearchTerm :: Lens Model MisoString
getSetSearchTerm =
    Lens
        searchTerm
        (\s model -> model { searchTerm = s })

type MessageIn = (Bool, MisoString) -- the boolean is passed through

data MessageOut
    = SearchResults (Bool, MisoString, [ CatalogPost ])
    | Mounted
    | UnMounted
    deriving Eq

instance ToJSON MessageOut where
    toJSON (SearchResults s) = object
        [ "tag" .= String "SearchResults"
        , "contents" .= toJSON s
        ]

    toJSON Mounted = object [ "tag" .= String "Mounted" ]
    toJSON UnMounted = object [ "tag" .= String "UnMounted" ]

instance FromJSON MessageOut where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString

        case tag of
            "Mounted"       -> pure Mounted
            "UnMounted"     -> pure UnMounted
            "SearchResults" -> SearchResults <$> m .: "contents"
            _               -> fail "Unknown SearchTypes.MessageOut tag"

    parseJSON _ = fail "Expected Object for SearchTypes.MessageOut"


searchInTopic :: Topic MessageIn
searchInTopic = topic "search-in"

searchOutTopic :: Topic MessageOut
searchOutTopic = topic "search-out"
