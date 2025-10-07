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
    , pushURI
    , consoleLog
    , consoleError
    , io_
    , get
    , modify
    , issue
    , subscribe
    , View
    )
import Servant.Miso.Router (route)
import Miso.String (MisoString, toMisoString)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Either (fromRight)
import Servant.API hiding (URI)
import Network.URI (unEscapeString)

import Common.FrontEnd.Action
import Common.FrontEnd.Model
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.Thread as Thread
import qualified Common.Utils as Utils
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import Common.Network.SiteType (fromCatalogPost)
import JSFFI.Saddle (encodeURIComponent)
import Common.FrontEnd.Routes

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
            , between_pages = True
            }
        )

    issue $ GetThread $ mkGetThread catalog_post

mainUpdate (OnErrorMessage msg) =
    io_ $ consoleError ("Main Component OnErrorMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse (Client.ReturnResult SenderLatest result)) =
    Utils.helper result $ \catalogPosts ->
        modify (\m -> m { catalog_posts = catalogPosts })

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
    modify (\m -> m { current_time = t, between_pages = True })
    publish Client.clientInTopic (SenderLatest, Client.FetchLatest t)

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " <> (toMisoString $ show board_thread_id)

    modify (\m -> m { between_pages = True })

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
    model <- get

    if not $ between_pages model then
        issue $ initialActionFromRoute model uri
    else
        modify (\m -> m { between_pages = False })

mainUpdate (SearchResults (searchTerm, catalogPosts)) = do
    model <- get

    modify (\m -> m { catalog_posts = catalogPosts, between_pages = True })

    io_ $ do
        consoleLog $ "Old URI:" <> (toMisoString $ show $ current_uri model)
        searchTermURIComponent <- encodeURIComponent searchTerm
        let new_uri = newCurrentURI model searchTermURIComponent
        consoleLog $ "SearchResults new uri: " <> (toMisoString $ show new_uri)
        pushURI new_uri

    where
        newCurrentURI :: Model -> MisoString -> URI
        newCurrentURI m searchTermURIComponent = (current_uri m)
            { uriPath = "search"
            , uriQueryString = Map.singleton
                "search"
                $ Just searchTermURIComponent
            }

mainUpdate (NotifySearch searchTerm) = publish Search.searchInTopic searchTerm


(</>) :: MisoString -> MisoString -> MisoString
(</>) a b = a <> "/" <> b


initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = fromRight NoAction routing_result
    where
        routing_result =
            route
                (Proxy :: Proxy (Route (View Model Action)))
                handlers
                (const uri)
                model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ =
            GetThread Client.GetThreadArgs
                { Client.website = toMisoString website
                , Client.board_pathpart = toMisoString board_pathpart
                , Client.board_thread_id = board_thread_id
                }

        h_search :: Maybe String -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | search_term m == unescaped_search_query =
                SearchResults (unescaped_search_query, [])
            | otherwise = NotifySearch $ unescaped_search_query

            where
                unescaped_search_query =
                    toMisoString $ unEscapeString $ search_query
