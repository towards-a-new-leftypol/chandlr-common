{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module Common.FrontEnd.Views
    ( catalogView
    , searchView
    , threadView
    , boardView
    , page404
    ) where

import Miso
    ( View
    , text
    , key_
    , mount_
    , mountWithProps
    , vfrag
    )
import Miso.Html.Property (class_)
import Miso.Html
    ( h1_
    , time_
    , div_
    , p_
    )
import Miso.String (MisoString, toMisoString)
import qualified Miso.String as Str
import Data.Text (Text)

import Common.FrontEnd.Model
import Common.FrontEnd.Action (Action (..))
import qualified Common.Component.Search as Search
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import qualified Common.Component.Thread as Thread
import qualified Common.Component.Thread.Model as Thread
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Routes (BoardThreadId)
import qualified Network.Client as Client
import Common.FrontEnd.Types (InitCtxRef)
import qualified Common.Admin.Component.DeleteIllegalPost as DIP
import qualified Common.Component.NavigationBar.View as Nav
import qualified Common.Component.NavigationBar as Nav
import qualified Common.Component.NavigationBar.Model as Nav
import Common.Cookies (CookieJar)

import Debug.Trace (trace)

timeControl :: Eq context => InitCtxRef -> View context Action
timeControl ctxRef = vfrag [ mount_ $ TC.app ctxRef ]


grid :: Eq context => Grid.Props -> View context action
grid = Grid.gridView Grid.app


search :: Eq context => View context Action
search = div_ [ key_ ("search" :: MisoString) ] [ mount_ Search.app ]


pageWrapperWithDefaults :: Eq context => InitCtxRef -> Model -> View context Action -> View context Action
pageWrapperWithDefaults ctxRef m inner_content =
    trace ("pageWrapperWithDefaults being called. Number of items in catalog_grid: " ++ (show $ length $ catalog_posts m)) $
    vfrag
        [ mount_ Client.app
        , mountWithProps (Thread.Props (admin m) (media_root_ m)) DIP.app
        -- , pre_ [] [ text $ "between_pages: " <> if between_pages then "True" else "False" ]
        , mountWithProps (Nav.Props (all_sites_and_boards m) (current_uri m)) $ Nav.app ctxRef
        , div_ [ class_ "page-inner-content" ] [ inner_content ]
        , Nav.supportingSvgs
        ]

commonCatalogView :: Eq context => InitCtxRef -> Model -> View context Action
commonCatalogView ctxRef m = pageWrapperWithDefaults ctxRef m $ vfrag
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text $ page_title m ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , timeControl ctxRef
    , search
    , grid (gridPropsFromModel m)
    ]

catalogView
    :: Eq context
    => InitCtxRef
    -> Maybe String
    -> Maybe CookieJar
    -> Model
    -> View context Action
catalogView ctxRef _ _ m = commonCatalogView ctxRef m

boardView
    :: Eq context
    => InitCtxRef
    -> a
    -> a
    -> Maybe CookieJar
    -> Model
    -> View context Action
boardView ctxRef _ _ _ m = commonCatalogView ctxRef m

searchView
    :: Eq context
    => InitCtxRef
    -> Maybe String
    -> Maybe CookieJar
    -> Model
    -> View context Action
searchView ctxRef _ _ m = pageWrapperWithDefaults ctxRef m $ vfrag
    [ div_
        [ class_ "page_heading" ]
        (
            h1_ [] [ text "Search" ]
            :
            if Str.null term then
                []
            else
                [ p_ [] [ text term ] ]
        )
    , search
    , grid (gridPropsFromModel m)
    ]

    where
        term = search_term m


threadView
    :: Eq context
    => InitCtxRef
    -> Text
    -> Text
    -> BoardThreadId
    -> Maybe CookieJar
    -> Model
    -> View context Action
threadView ctxRef _site_name _board_pathpart _board_thread_id _cookies m =
    pageWrapperWithDefaults ctxRef m $ vfrag
        [ mountWithProps
            (Thread.Props (admin m) (media_root_ m))
            (Thread.app ctxRef)
        ]


page404 :: View model Action
page404 = h1_ [] [ text "404 Not Found" ]


gridPropsFromModel :: Model -> Grid.Props
gridPropsFromModel m = Grid.Props (catalog_posts m) (media_root_ m)
