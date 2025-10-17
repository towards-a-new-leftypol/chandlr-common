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
  , defaultEvents
  , modify
  , issue
  , io_
  , get
  , publish
  , subscribe
  )
import qualified Miso as M
import Miso.Binding ((-->))

import Common.Component.Search.SearchTypes
import Common.Component.Search.View
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils
import qualified Common.FrontEnd.Model as FE

pattern Sender :: Client.Sender
pattern Sender = "search"

update :: Action -> Effect parent Model Action
update Initialize = do
    io_ $ consoleLog "Search component Initialize!"
    subscribe Client.clientOutTopic SearchResult OnMessageError
    subscribe searchInTopic OnMessage OnMessageError

update (SearchChange q) =
    modify (\m -> m { searchTerm = q })

update OnSubmit = do
    model <- get

    let search_query = searchTerm model

    io_ $ consoleLog $ "Submit! " <> search_query

    publish Client.clientInTopic (Sender, Client.Search search_query)

update (ChangeAndSubmit search_query) = do
    issue $ SearchChange search_query
    issue OnSubmit

update (SearchResult (Client.ReturnResult Sender result)) = do
    io_ $ consoleLog "Search - SearchResult action handler"
    Utils.helper result $ \searchResults -> do
        model <- get
        publish searchOutTopic (searchTerm model, searchResults)

update (SearchResult (Client.ReturnResult _ _)) = return ()

update (OnMessageError msg) =
    io_ $ consoleError msg

update (OnMessage query) = issue $ ChangeAndSubmit query

app :: Component FE.Model Model Action
app = M.Component { M.model = Model ""
    , M.hydrateModel = Nothing
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.bindings = [ FE.getSetSearchTerm --> getSetSearchTerm ]
    }
