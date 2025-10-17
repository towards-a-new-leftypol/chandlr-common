{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.MainComponent where

import Miso
    ( App
    , View
    , LogLevel (DebugAll)
    , defaultEvents
    , uriSub
    , URI (..)
    )
import qualified Miso as M
import Miso.String (toMisoString, MisoString)
import Data.Proxy
import Servant.API hiding (URI)
import Servant.Miso.Router (route)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import Common.FrontEnd.JSONSettings (JSONSettings (..))
import qualified Common.Component.CatalogGrid as Grid
import Common.Component.TimeControl (earliest)
import Common.FrontEnd.Routes
import Common.FrontEnd.Model
import Common.FrontEnd.Views
import Common.FrontEnd.Action
import Common.FrontEnd.Types
import Utils (pageTypeFromURI, PageType (..))
#if defined(FRONT_END)
import Common.FrontEnd.Update
import Miso (JSM)
import Control.Monad.IO.Class (liftIO)
#endif

type MainComponent = App Model Action

app :: InitCtxRef -> MainComponent
app ctxRef =
    M.Component
        { M.model         = emptyModel
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
        , M.initialAction = Just (Initialize ctxRef)
        , M.mountPoint    = Nothing
        , M.logLevel      = DebugAll
        , M.scripts = []
        , M.mailbox = const Nothing
        , M.bindings = []
        }

    where
        emptyModel :: Model
        emptyModel = Model
            { current_uri = URI "" "" Map.empty
            , media_root_ = ""
            , current_time = earliest
            , search_term = ""
            , initial_action = NoAction
            , thread_message = Nothing
            , pg_api_root = ""
            , client_fetch_count = 0
            , catalog_posts = []
            , between_pages = False
            }


#ifdef FRONT_END
initializeModel :: InitCtxRef -> JSM Model
initializeModel ctxRef = liftIO $ do
#else
initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
#endif
    putStrLn "MainComponent initializeModel"
    ctx <- readIORef ctxRef

    let settings = init_settings ctx
    let initialPayload = init_payload ctx
    let uri = init_uri ctx

    return
          Model
              { current_uri = init_uri ctx
              , media_root_ = toMisoString $ media_root settings
              , current_time = timestamp initialPayload
              , search_term = termFromUri uri
              , initial_action = NoAction
              , thread_message = Nothing
              , pg_api_root = toMisoString $ postgrest_url settings
              , client_fetch_count = postgrest_fetch_count settings
              , catalog_posts = Grid.initialItems $ initialData initialPayload
              , between_pages = False
              }

    where
        termFromUri :: URI -> MisoString
        termFromUri u =
            case pageTypeFromURI u of
                Search (Just q) -> q
                _ -> ""


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
