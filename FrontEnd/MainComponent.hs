{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.MainComponent where

import Miso
    ( App
    , View
    , LogLevel (DebugAll)
    , defaultEvents
    , uriSub
    )
import qualified Miso as M
import Miso.String (toMisoString)
import Data.Proxy
import Servant.API
import Servant.Miso.Router (route)
import Data.IORef (readIORef)

import Common.FrontEnd.JSONSettings (JSONSettings (..))
import qualified Common.Component.CatalogGrid as Grid
import Common.FrontEnd.Routes
import Common.FrontEnd.Model
import Common.FrontEnd.Views
import Common.FrontEnd.Action
import Common.FrontEnd.Types
#if defined(FRONT_END)
import Common.FrontEnd.Update
#endif

type MainComponent = App Model Action

app :: InitCtxRef -> MainComponent
app ctxRef =
    M.Component
        { M.model         = Uninitialized
#if defined(FRONT_END)
        , M.update        = mainUpdate
#else
        , M.update        = undefined
#endif
        , M.hydrateModel  = Just $ initializeModel ctxRef
        , M.view          = mainView ctxRef
        , M.subs          = [ uriSub ChangeURI ]
        , M.events        = defaultEvents
        , M.styles = []
        , M.initialAction = Just Initialize
        , M.mountPoint    = Nothing
        , M.logLevel      = DebugAll
        , M.scripts = []
        , M.mailbox = const Nothing
        , M.bindings = []
        }


initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
    putStrLn "MainComponent initializeModel"
    ctx <- readIORef ctxRef

    let settings = init_settings ctx
    let initialPayload = init_payload ctx

    return
          Model
              { current_uri = init_uri ctx
              , media_root_ = toMisoString $ media_root settings
              , current_time = timestamp initialPayload
              , search_term = "" -- TODO: get this from URL
              , initial_action = NoAction
              , thread_message = Nothing
              , pg_api_root = toMisoString $ postgrest_url settings
              , client_fetch_count = postgrest_fetch_count settings
              , catalog_posts = Grid.initialItems $ initialData initialPayload
              , between_pages = False
              }


mainView :: InitCtxRef -> Model -> View Model Action
mainView ctxRef model = mainView_
    where
        mainView_ = either (const page404) id $
            route
                (Proxy :: Proxy (Route (View Model Action)))
                handlers
                current_uri
                model

        handlers
            =    catalogView ctxRef
            :<|> threadView ctxRef
            :<|> searchView ctxRef
