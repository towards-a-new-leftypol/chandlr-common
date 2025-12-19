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
    , onMounted
    , onUnmounted
    , key_
    , mount_
    , mount
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

timeControl :: InitCtxRef -> View Model Action
timeControl ctxRef =
    div_ [ key_ ("time-control" :: MisoString) ] [ mount $ TC.app ctxRef ]


grid :: InitCtxRef -> View Model Action
grid ctxRef = div_
    [ key_ ("catalog-grid" :: MisoString)
    , onMounted CatalogViewMounted
    ]
    [ mount $ Grid.app ctxRef ]


search :: View Model Action
search = div_ [ key_ ("search" :: MisoString) ] [ mount Search.app ]


pageWrapperWithDefaults :: Model -> View model Action -> View model Action
pageWrapperWithDefaults _ inner_content =
    div_ [ key_ ("top-level" :: MisoString) ]
        [ div_
            [ onMounted ClientMounted
            , onUnmounted ClientUnmounted
            , key_ ("http-client" :: MisoString)
            ]
            [ mount Client.app ]
        , div_
            [ key_ ("delete-illegal-post" :: MisoString) ]
            [ mount DIP.app ]
        -- , pre_ [] [ text $ "between_pages: " <> if between_pages then "True" else "False" ]
        , inner_content
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
            [ key_ ("thread-view" :: MisoString) ]
            [ mount_
                [ onMounted ThreadViewMounted
                ]
                (Thread.app ctxRef)
            ]


page404 :: View model Action
page404 = h1_ [] [ text "404 Not Found" ]
