{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.Views
    ( catalogView
    , threadView
    , searchView
    , page404
    ) where

import Miso
    ( View
    , div_
    , class_
    , h1_
    , time_
    , text
    )
import Miso.String (toMisoString)
import Data.Text (Text)

import Common.FrontEnd.Model
import Common.FrontEnd.Action (Action)
import qualified Common.Component.ThreadView as Thread
import qualified Common.Component.Search.View as Search
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Routes (BoardThreadId)
import Common.FrontEnd.Interfaces

catalogView :: Model -> View Action
catalogView m = div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Overboard Catalog" ]
        , time_ [] [ text $ toMisoString $ show $ current_time m ]
        ]
    , TC.view iTime (tc_model m)
    , Search.view iSearch (search_model m)
    , Grid.view iGrid (grid_model m)
    ]

threadView :: Text -> Text -> BoardThreadId -> Model -> View Action
threadView site_name board_pathpart board_thread_id m = maybe
    (h1_ [] [ text "Thread View" ])
    Thread.view
    (thread_model m)

searchView :: Maybe Text -> Model -> View Action
searchView _ m = div_ []
    [ div_
        [ class_ "page_heading" ]
        [ h1_ [] [ text "Search" ]
        , time_ [] [ text $ Search.searchTerm $ search_model m ]
        ]
    , Search.view iSearch (search_model m)
    , Grid.view iGrid $ (grid_model m)
            { Grid.display_items = (Search.displayResults (search_model m))
            }
    ]

page404 :: View Action
page404 = h1_ [] [ text "404 Not Found" ]
