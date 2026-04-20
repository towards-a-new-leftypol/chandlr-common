{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}

module Common.FrontEnd.Views
    ( catalogView
    , searchView
    , threadView
    , page404
    ) where

import Miso
    ( View
    , text
    , key_
    , mount_
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
import qualified Common.Component.Thread as Thread
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Routes (BoardThreadId)
import qualified Network.Client as Client
import Common.FrontEnd.Types (InitCtxRef)
import qualified Common.Admin.Component.DeleteIllegalPost as DIP
import qualified Common.Component.NavigationBar.View as Nav

import Debug.Trace (trace)

timeControl :: InitCtxRef -> View Model Action
timeControl ctxRef =
    div_ [ key_ ("time-control" :: MisoString) ] [ mount_ $ TC.app ctxRef ]


grid :: InitCtxRef -> View Model Action
grid ctxRef = mount_ (Grid.app ctxRef)


search :: View Model Action
search = div_ [ key_ ("search" :: MisoString) ] [ mount_ Search.app ]


pageWrapperWithDefaults :: Model -> View model Action -> View model Action
pageWrapperWithDefaults m inner_content =
    trace ("pageWrapperWithDefaults being called. Number of items in catalog_grid: " ++ (show $ length $ catalog_posts m)) $
    div_ [ key_ ("top-level" :: MisoString) ]
        [ div_
            []
            [ mount_ Client.app ]
        , div_
            [ key_ ("delete-illegal-post" :: MisoString) ]
            [ mount_ DIP.app ]
        -- , pre_ [] [ text $ "between_pages: " <> if between_pages then "True" else "False" ]
        , Nav.navbar
        , div_ [ class_ "page-inner-content" ] [ inner_content ]
        , Nav.supportingSvgs
        ]


catalogView :: InitCtxRef -> Maybe String -> Model -> View Model Action
catalogView ctxRef _ m = pageWrapperWithDefaults m $ div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Overboard Catalog" ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , timeControl ctxRef
    , search
    , grid ctxRef
    ]


searchView :: InitCtxRef -> Maybe String -> Model -> View Model Action
searchView ctxRef _ m = pageWrapperWithDefaults m $ div_ []
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
    , grid ctxRef
    ]

    where
        term = search_term m


threadView :: InitCtxRef -> Text -> Text -> BoardThreadId -> Model -> View Model Action
threadView ctxRef site_name board_pathpart board_thread_id m =
    pageWrapperWithDefaults m $
        div_
            []
            [ mount_ (Thread.app ctxRef) ]


page404 :: View model Action
page404 = h1_ [] [ text "404 Not Found" ]
