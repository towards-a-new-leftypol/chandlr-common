{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Update where

import Miso
    ( URI (..)
    , Effect
    , ROOT
    , publish
    , put
    , pushURI
    , consoleLog
    , consoleError
    , io_
    , get
    , modify
    , issue
    , subscribe
    )

import Miso.String (MisoString, fromMisoString, toMisoString)
import qualified Data.Map.Strict as Map
import Network.URI (escapeURIString, isAllowedInURI)

import Common.FrontEnd.Action
import Common.FrontEnd.Model
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.Thread as Thread
import qualified Common.Utils as Utils
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import Common.Network.SiteType (fromCatalogPost)

pattern Sender :: Client.Sender
pattern Sender = "main"

pattern SenderLatest :: Client.Sender
pattern SenderLatest = "main-latest"

pattern SenderThread :: Client.Sender
pattern SenderThread = "main-thread"


mainUpdate :: Action -> Effect ROOT Model Action
mainUpdate NoAction = return ()
mainUpdate Initialize = do
    subscribe Client.clientOutTopic ClientResponse OnErrorMessage
    subscribe Grid.catalogOutTopic GridMessage OnErrorMessage
    subscribe Search.searchOutTopic SearchResults OnErrorMessage

mainUpdate ClientMounted = do
    model <- get

    io_ $ consoleLog "Http Client Mounted!"

    publish
        Client.clientInTopic
        ( Sender
        , Client.InitModel $
            Client.Model
                (pg_api_root model)
                (client_fetch_count model)
        )

    issue $ initial_action model
    modify $ \m -> m { initial_action = NoAction }

mainUpdate ClientUnmounted = io_ $ consoleLog "Http Client Unmounted!"

mainUpdate ThreadViewMounted = do
    io_ $ consoleLog "ThreadViewMounted"

    model <- get

    maybe
        (io_ $ consoleLog "No thread_message available for sending in Main Model")
        (publish Thread.threadTopic)
        (thread_message model)

mainUpdate (GridMessage (Grid.SelectThread catalog_post)) = do
    io_ $ consoleLog "GridMessage!"

    modify
        ( \m -> m
            { thread_message = Just $
                Thread.RenderSite (media_root_ m) (fromCatalogPost catalog_post)
            }
        )

    issue $ GetThread $ mkGetThread catalog_post

mainUpdate (OnErrorMessage msg) =
    io_ $ consoleError ("Main Component OnErrorMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse (Client.ReturnResult SenderLatest result)) =
    Utils.helper result $ \catalogPosts ->
        publish Grid.catalogInTopic $ Grid.DisplayItems catalogPosts

mainUpdate (ClientResponse (Client.ReturnResult SenderThread result)) = do
    io_ $ consoleLog $ SenderThread <> " - Has result. Storing result in model."

    Utils.helper result $ \sites -> do
        modify
            ( \m -> m
                { thread_message = Just $
                    Thread.RenderSite (media_root_ m) (head sites)
                }
            )
        issue ThreadViewMounted

mainUpdate (ClientResponse (Client.ReturnResult _ _)) = return ()

mainUpdate (GoToTime t) = do
    modify (\m -> m { current_time = t })
    publish Client.clientInTopic (SenderLatest, Client.FetchLatest t)

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " <> (toMisoString $ show board_thread_id)

    model <- get

    io_ $ pushURI $ new_current_uri model

    publish Client.clientInTopic (SenderThread, Client.GetThread Client.GetThreadArgs {..})

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = website
                    </> board_pathpart
                    </> (toMisoString $ show board_thread_id)
            , uriQueryString = Map.empty
            }

mainUpdate (ChangeURI uri) = do
    modify (\m -> m { current_uri = uri })
    io_ $ consoleLog $ "ChangeURI! " <> (toMisoString $ show uri)


mainUpdate (SearchResults (searchTerm, catalogPosts)) = do
    model <- get

    let new_uri :: URI = new_current_uri model

    publish Grid.catalogInTopic $ Grid.DisplayItems catalogPosts

    io_ $ do
        consoleLog $ "Old URI:" <> (toMisoString $ show $ current_uri model)
        consoleLog $ "SearchResults new uri: " <> (toMisoString $ show new_uri)
        -- pushURI new_uri

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = "search"
            , uriQueryString = Map.singleton
                "search"
                $ Just
                    (toMisoString $ escapeURIString isAllowedInURI $ fromMisoString searchTerm)
            }

mainUpdate (NotifySearch searchTerm) = publish Search.searchInTopic searchTerm


(</>) :: MisoString -> MisoString -> MisoString
(</>) a b = a <> "/" <> b
