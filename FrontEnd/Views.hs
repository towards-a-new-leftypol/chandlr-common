{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Views
    ( catalogView
    , searchView
    , threadView
    , page404
    ) where

import Miso
    ( View
    , text
    , onMountedWith
    , onUnmountedWith
    , key_
    , mount
    )
import Miso.Html.Property (class_)
import Miso.Html
    ( h1_
    , time_
    , div_
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

timeControl :: TimeControl model -> View model Action
timeControl = mount (div_ [ key_ ("time-control" :: MisoString) ])


grid :: Grid.GridComponent model -> View model Action
grid = mount (div_ [ key_ ("catalog-grid" :: MisoString) ])


search :: View model Action
search = mount (div_ [ key_ ("search" :: MisoString) ]) Search.app


pageWrapperWithDefaults :: View model Action -> View model Action
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


catalogView :: TimeControl model -> Grid.GridComponent model -> Model -> View model Action
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


searchView :: Grid.GridComponent model -> Maybe String -> Model -> View model Action
searchView gc _ m = pageWrapperWithDefaults $ div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Search" ]
        , time_ [] [ text $ search_term m ]
        ]
    , search
    , grid gc
    ]



threadView :: Thread.Model -> Text -> Text -> BoardThreadId -> Model -> View model Action
threadView thread_model site_name board_pathpart board_thread_id m =
    pageWrapperWithDefaults $ mount
        (div_ [ onMountedWith (const ThreadViewMounted), key_ ("thread-view" :: MisoString) ])
        (Thread.app thread_model)


page404 :: View model Action
page404 = h1_ [] [ text "404 Not Found" ]
