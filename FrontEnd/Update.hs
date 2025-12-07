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
    , Topic
    , topic
    )
import Miso.Subscription.History (replaceURI)
import Servant.Miso.Router (route)
import Miso.String (MisoString, toMisoString)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import Data.Either (fromRight)
import Servant.API hiding (URI)
import Network.URI (unEscapeString)
import Data.IORef (modifyIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

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
import qualified Common.FrontEnd.Types as T
import qualified Common.Network.CatalogPostType as CatPost
import qualified Common.Component.TimeControl as TC

import JSFFI.Profile (sectionEnd, toJSString, displayTotals)

pattern Sender :: Client.ReturnTopicName
pattern Sender = "main"

pattern SenderLatest :: Client.ReturnTopicName
pattern SenderLatest = "main-latest"

pattern SenderThread :: Client.ReturnTopicName
pattern SenderThread = "main-thread"


mainUpdate :: Action -> Effect ROOT Model Action
mainUpdate NoAction = return ()
mainUpdate (Initialize ctxRef) = do
    subscribe clientLatestReturnTopic (ClientResponse SenderLatest) OnErrorMessage
    subscribe clientThreadReturnTopic (ClientResponse SenderThread) OnErrorMessage
    subscribe Grid.catalogOutTopic GridMessage OnErrorMessage
    subscribe Search.searchOutTopic SearchResults OnErrorMessage
    subscribe TC.timeControlTopic GoToTime OnErrorMessage

    model <- get

    io_ $ do
        consoleLog "MainComponent Initialize action"
        consoleLog $ "MainComponent pg_api_root: " <> (pg_api_root model)
    -- collect garbage
    io_ $ liftIO $ modifyIORef ctxRef
        ( \ctx@T.AppInitCtx {..} -> ctx
            { T.init_payload =
                init_payload { T.initialData = T.Nil }
            }
        )

    where
        clientLatestReturnTopic :: Topic Client.MessageOut
        clientLatestReturnTopic = topic SenderLatest

        clientThreadReturnTopic :: Topic Client.MessageOut
        clientThreadReturnTopic = topic SenderThread

mainUpdate ClientMounted = do
    model <- get

    io_ $ do
        consoleLog "Http Client Mounted!"
        consoleLog $ "pg_api_root: " <> pg_api_root model
        consoleLog $ "client_fetch_count: " <> (toMisoString $ client_fetch_count model)

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

mainUpdate CatalogViewMounted = do
    io_ $ do
        consoleLog "CatalogViewMounted"
        liftIO $ sectionEnd $ toJSString "pageLoad"
        liftIO $ displayTotals

mainUpdate ThreadViewMounted = do
    io_ $ do
        consoleLog "ThreadViewMounted"
        liftIO $ sectionEnd $ toJSString "pageLoad"
        liftIO $ displayTotals

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

    model <- get

    io_ $ do
        consoleLog $ "calling pushURI on " <> toMisoString (show (new_current_uri model))
        pushURI $ new_current_uri model

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = CatPost.site_name catalog_post
                    </> CatPost.pathpart catalog_post
                    </> toMisoString (show $ CatPost.board_thread_id catalog_post)
            , uriQueryString = Map.empty
            }


mainUpdate (OnErrorMessage msg) =
    io_ $ consoleError ("Main Component OnErrorMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse SenderLatest (Client.ReturnResult result)) =
    Utils.helper result $
        \catalogPosts -> modify
            ( \m -> m
                { catalog_posts = catalogPosts
                , between_pages = False
                }
            )

mainUpdate (ClientResponse SenderThread (Client.ReturnResult result)) = do
    io_ $ consoleLog $ SenderThread <> " - Has result. Storing result in model."

    Utils.helper result $ \sites -> do
        modify
            ( \m -> m
                { thread_message = Just $
                    Thread.RenderSite (media_root_ m) (head sites)
                }
            )
        issue ThreadViewMounted

mainUpdate (ClientResponse _ (Client.ReturnResult _)) = return ()

mainUpdate (GoToTime t) = do
    modify (\m -> m { current_time = t, between_pages = True })
    publish Client.clientInTopic (SenderLatest, Client.FetchLatest t)

    model <- get

    io_ $ do
        consoleLog $ "calling replaceURI on " <> toMisoString (show (new_current_uri model))
        replaceURI $ new_current_uri model

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriQueryString = Map.fromList [ ("t", Just $ toMisoString $ show t) ]
            }

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " <> (toMisoString $ show board_thread_id)

    modify (\m -> m { between_pages = True })

    publish Client.clientInTopic (SenderThread, Client.GetThread Client.GetThreadArgs {..})


mainUpdate (ChangeURI uri) = do
    modify (\m -> m { current_uri = uri })
    io_ $ consoleLog $ "ChangeURI! " <> (toMisoString $ show uri)
    model <- get

    if not $ between_pages model then do
        io_ $ consoleLog $ "Not between pages, issuing initialAction, between_pages: " <> (toMisoString $ show $ (between_pages model))
        issue $ initialActionFromRoute model uri
    else do
        io_ $ consoleLog "Between pages."
        modify (\m -> m { between_pages = False })

mainUpdate (SearchResults (intendPushUri, searchTerm, catalogPosts)) = do
    io_ $ consoleLog $ "MainComponent - SearchResults. intendPushUri: " <> (toMisoString $ show intendPushUri) <> ", searchTerm: " <> searchTerm <> ", number of results: " <> (toMisoString $ show $ length catalogPosts)
    modify (\m -> m { catalog_posts = catalogPosts, between_pages = intendPushUri })

    model <- get

    when intendPushUri $ io_ $ do
        consoleLog $ "Old URI:" <> (toMisoString $ show $ current_uri model) <> " searchTerm: " <> searchTerm
        searchTermURIComponent <- encodeURIComponent searchTerm
        let new_uri = newCurrentURI model searchTermURIComponent
        consoleLog $ "SearchResults pushURI new uri: " <> (toMisoString $ show new_uri)
        pushURI new_uri

    where
        newCurrentURI :: Model -> MisoString -> URI
        newCurrentURI m searchTermURIComponent = (current_uri m)
            { uriPath = "search"
            , uriQueryString = Map.singleton
                "q"
                $ Just searchTermURIComponent
            }

mainUpdate (NotifySearch (b, searchTerm)) = do
    io_ $ consoleLog $ "NotifySearch " <> searchTerm
    modify (\m -> m { search_term = searchTerm })
    publish Search.searchInTopic (b, searchTerm)


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

        h_latest :: Maybe String -> Model -> Action
        h_latest Nothing m = GoToTime $ current_time m
        h_latest (Just t) _ = GoToTime $ read t

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ =
            GetThread Client.GetThreadArgs
                { Client.website = toMisoString website
                , Client.board_pathpart = toMisoString board_pathpart
                , Client.board_thread_id = board_thread_id
                }

        h_search :: Maybe String -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) _ = NotifySearch (False, unescaped_search_query)
            where
                unescaped_search_query =
                    toMisoString $ unEscapeString $ search_query
