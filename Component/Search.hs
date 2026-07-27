{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Component.Search
  ( view
  , update
  , Model (..)
  , Action (..)
  , app
  ) where

import Miso
  ( Effect
  , consoleLog
  , consoleError
  , Component
  , modify
  , issue
  , io_
  , get
  , publish
  , subscribe
  , Topic
  , topic
  )
import qualified Miso as M

import Common.Component.Search.SearchTypes
import Common.Component.Search.View
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils


pattern ReturnTopic :: Client.ReturnTopicName
pattern ReturnTopic = "search-results"

update :: Action -> Effect context props Model Action
update Initialize = do
    io_ $ consoleLog "Search component Initialize!"
    subscribe clientReturnTopic SearchResult OnMessageError
    subscribe searchInTopic OnMessage OnMessageError
    io_ $ publish searchOutTopic Mounted

    where
        clientReturnTopic :: Topic Client.MessageOut
        clientReturnTopic = topic ReturnTopic

update OnUnmount = io_ $ publish searchOutTopic UnMounted

update (SearchChange q) =
    modify (\m -> m { searchTerm = q })

update Submit = do
    modify (\m -> m { intendPushUri = True })
    issue OnSubmit

update OnSubmit = do
    model <- get

    let search_query = searchTerm model

    io_ $ do
        consoleLog $ "Submit! " <> search_query
        publish Client.clientInTopic (ReturnTopic, Client.Search search_query)

update (ChangeAndSubmit search_query) = do
    issue $ SearchChange search_query
    issue OnSubmit

update (SearchResult (Client.ReturnResult result)) = do
    io_ $ consoleLog "Search - SearchResult action handler"

    Utils.helper result $ \searchResults -> do
        model <- get
        io_ $ publish
            searchOutTopic $ SearchResults
                ( intendPushUri model
                , searchTerm model
                , searchResults
                )

update (OnMessageError msg) =
    io_ $ consoleError msg

update (OnMessage (b, query)) = do
    io_ $ consoleLog "Search OnMessage"
    modify (\m -> m { intendPushUri = b })
    issue $ ChangeAndSubmit query

app :: Component context props Model Action
app = M.Component
    { M.model = Model "" False
    , M.hydrateModel = Nothing
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.styles = []
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.eventPropagation = False
    , M.mount = Just Initialize
    , M.unmount = Just OnUnmount
    , M.onPropsChanged = Nothing
    , M.useContext = False
    }
