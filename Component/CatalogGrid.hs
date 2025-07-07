{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Common.Component.CatalogGrid
( Model (..)
, initialModel
, Action (..)
, Interface (..)
, view
, update
, app
, GridComponent
) where

import Control.Monad.State (modify)
import Data.Maybe (maybeToList)
import Data.Either (fromRight)
import Data.Text (pack, Text)
import qualified Data.Text as T
import Miso
    ( View, div_ , class_ , img_ , href_ , a_
    , src_ , title_ , b_ , span_
    , p_ , id_ , Effect
    , text, rawHtml, onWithOptions
    , defaultOptions, preventDefault
    , Attribute, emptyDecoder
    , notify
    , io_
    , consoleLog
    , Component
    , key_
    )
import Miso.String (toMisoString, MisoString)
import qualified Miso as M

import Common.Network.CatalogPostType (CatalogPost, board_post_id)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Parsing.EmbedParser (extractVideoId)
import Common.FrontEnd.Action (mkGetThread)
import qualified Common.FrontEnd.MainComponent as MC
import Common.Component.Grid.Types

initialModel :: MisoString -> Model
initialModel media_root_ = Model
    { display_items = []
    , media_root = toMisoString media_root_
    }

app
    :: MC.MainComponent
    -> MisoString
    -> GridComponent
app mc m_root =
    M.Component
        { M.model = initialModel m_root
        , M.update = update mc
        , M.view = view
        , M.subs = []
        , M.events = M.defaultEvents
        , M.styles = []
        , M.initialAction = Nothing
        , M.mountPoint = Nothing
        , M.logLevel = M.DebugAll
        }


-- Custom event handler with preventDefault set to True
onClick_ :: a -> Attribute a
onClick_ action = onWithOptions defaultOptions { preventDefault = True } "click" emptyDecoder (const $ const action)


update
    :: MC.MainComponent
    -> Action
    -> Effect Model Action
update _ (DisplayItems xs) = do
    io_ $ consoleLog "CatalogGrid - DisplayItems"
    modify $ \m -> (m { display_items = xs })
update mc (ThreadSelected post) = io_ $ notify mc $ mkGetThread post


view :: Model -> View Action
view model =
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ div_
                [ id_ "Grid" ]
                ( if null (display_items model)
                  then []
                  else [ (gridItem model) (head $ display_items model) ]
                )
            ]
        ]

gridItem :: Model -> CatalogPost -> View Action
gridItem _ post =
    div_
        [ class_ "thread grid-li grid-size-small", key_ $ show $ board_post_id post ]
        [ a_
            [ onClick_ (ThreadSelected post)
            ]
            [ text "TEST" ]
        ]
