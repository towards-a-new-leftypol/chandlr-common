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
    , component_
    )
import Miso.String (toMisoString)
import Data.Text (Text)

import Common.FrontEnd.Model
import Common.FrontEnd.Action (Action)
import qualified Component.Search as Search
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.Thread as Thread
import Common.Component.TimeControl (TimeControl)
import Common.FrontEnd.Routes (BoardThreadId)

catalogView :: TimeControl -> Grid.GridComponent -> Model -> View Action
catalogView tc gc m = div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Overboard Catalog" ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , component_ tc []
    , component_ Search.app []
    , component_ gc []
    ]

searchView :: Grid.GridComponent -> Maybe Text -> Model -> View Action
searchView gc _ m = div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Search" ]
        , time_ [] [ text $ search_term m ]
        ]
    , component_ Search.app []
    , component_ gc []
    ]

threadView :: Thread.ThreadComponent -> Text -> Text -> BoardThreadId -> Model -> View Action
threadView threadComponent site_name board_pathpart board_thread_id m =
    component_ threadComponent []

page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]
