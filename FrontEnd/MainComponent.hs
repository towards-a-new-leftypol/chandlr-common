{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.MainComponent where

import Miso
    ( App
    , View
    , URI (..)
    , LogLevel (DebugAll)
    , defaultEvents
    , uriSub
    )
import qualified Miso as M
import Miso.String (toMisoString)
import Data.Proxy
import Servant.API hiding (URI)
import Servant.Miso.Router (route)

import Common.FrontEnd.JSONSettings (JSONSettings (..))
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.Thread as Thread
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Routes
import Common.FrontEnd.Model
import Common.FrontEnd.Views
import Common.FrontEnd.Action
import Common.FrontEnd.Types
#if defined(FRONT_END)
import Miso (JSM, consoleLog)
import Common.FrontEnd.Update
import Utils
    ( pageTypeFromURI
    , PageType (..)
    , getInitialDataPayload
    )
#endif

type MainComponent = App Model Action

app :: JSONSettings -> URI -> InitialDataPayload -> MainComponent
app settings url pagePayload =
    M.Component
        { M.model         = initialModel
#if defined(FRONT_END)
        , M.hydrateModel  = Just $ getInitCatalogPosts initialModel
        , M.update        = mainUpdate
#else
        , M.hydrateModel  = Nothing
        , M.update        = undefined
#endif
        , M.view          = mainView (initialData pagePayload)
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

    where
        initialModel :: Model
        initialModel =
          Model
              { current_uri = url
              , media_root_ = toMisoString $ media_root settings
              , current_time = timestamp pagePayload
              , search_term = "" -- TODO: get this from URL
              , initial_action = NoAction
              , thread_message = Nothing
              , pg_api_root = toMisoString $ postgrest_url settings
              , client_fetch_count = postgrest_fetch_count settings
              , catalog_posts = []
              , between_pages = False
              }

#if defined(FRONT_END)
getInitCatalogPosts :: Model -> URI -> JSM Model
getInitCatalogPosts m uri = do
    consoleLog "MainComponent getInitCatalogPosts"

    if pageType == Catalog || pageType == Search then do
        initialPayload <- getInitialDataPayload
        consoleLog $ "Found " <> toMisoString (show $ length $ Grid.initialItems $ initialData initialPayload) <> " catalog posts in page data."
        return $ m { catalog_posts = Grid.initialItems $ initialData initialPayload }

    else
        return m

    where
        pageType = pageTypeFromURI uri
#endif


mainView :: InitialData -> Model -> View Model Action
mainView initial_data model = mainView_
    where
        mainView_ = either (const page404) id $
            route (Proxy :: Proxy (Route (View Model Action))) handlers current_uri model

        handlers
            =    (catalogView tc grid)
            :<|> (threadView $ thread_model initial_data)
            :<|> (searchView grid)

        tc :: TC.TimeControl Model
        tc = TC.app 0

        thread_model :: InitialData -> Thread.Model
        thread_model (ThreadData site posts_w_bodies) =
            Thread.Model
                { Thread.site = site
                , Thread.media_root = media_root_ model
                , Thread.post_bodies = posts_w_bodies
                , Thread.current_time = current_time model
                }
        thread_model _ = Thread.Uninitialized

        grid :: Grid.GridComponent Model
        grid = Grid.app (media_root_ model) initial_data
