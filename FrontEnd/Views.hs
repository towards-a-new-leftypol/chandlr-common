{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Views
    ( catalogView
    , searchView
    , threadView
    , page404
    ) where

import Miso
    ( View
    , div_
    , class_
    , h1_
    , time_
    , text
    , onMountedWith
    , onUnmountedWith
    , key_
    , mount
    )
import Miso.String (MisoString, toMisoString)
import Data.Text (Text)

import Common.FrontEnd.Model
import Common.FrontEnd.Action (Action (..))
import qualified Common.Component.Search as Search
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.Thread as Thread
import Common.Component.TimeControl (TimeControl)
import Common.FrontEnd.Routes (BoardThreadId)
import qualified Network.Client as Client

timeControl :: TimeControl -> View Action
timeControl = mount (div_ [ key_ ("time-control" :: MisoString) ])


grid :: Grid.GridComponent -> View Action
grid = mount (div_ [ key_ ("catalog-grid" :: MisoString) ])


search :: View Action
search = mount (div_ [ key_ ("search" :: MisoString) ]) Search.app


pageWrapperWithDefaults :: View Action -> View Action
pageWrapperWithDefaults inner_content =
    div_ [ key_ ("top-level" :: MisoString) ]
        [ mount
            (div_
                [ onMountedWith (const ClientMounted)
                , onUnmountedWith (const ClientUnmounted)
                , key_ ("http-client" :: MisoString)
                ]
            )
            Client.app
        , inner_content
        ]


catalogView :: TimeControl -> Grid.GridComponent -> Model -> View Action
catalogView tc gc m = pageWrapperWithDefaults $ div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Overboard Catalog" ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , timeControl tc
    , search
    , grid gc
    ]


searchView :: Grid.GridComponent -> Maybe String -> Model -> View Action
searchView gc _ m = pageWrapperWithDefaults $ div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Search" ]
        , time_ [] [ text $ search_term m ]
        ]
    , search
    , grid gc
    ]



threadView :: Thread.Model -> Text -> Text -> BoardThreadId -> Model -> View Action
threadView thread_model site_name board_pathpart board_thread_id m =
    pageWrapperWithDefaults $ mount
        (div_ [ onMountedWith (const ThreadViewMounted), key_ ("thread-view" :: MisoString) ])
        (Thread.app thread_model)


page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]
