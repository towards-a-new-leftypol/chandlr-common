{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Common.Component.CatalogGrid
( Model (..)
, initialModel
, Action (..)
, view
, update
, app
, mkApp
, GridComponent
, InMessage (..)
, OutMessage (..)
, catalogInTopic
, catalogOutTopic
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
    , publish
    , subscribe
    , io_
    , consoleError
    , consoleLog
    )
import Miso.String (toMisoString, MisoString)
import qualified Miso as M

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Parsing.EmbedParser (extractVideoId)
import Common.Component.CatalogGrid.GridTypes

initialModel :: MisoString -> Model
initialModel media_root_ = Model
    { display_items = []
    , media_root = media_root_
    }

mkApp :: Model -> GridComponent
mkApp model =
    M.Component
        { M.model = model
        , M.update = update
        , M.view = view
        , M.subs = []
        , M.events = M.defaultEvents
        , M.styles = []
        , M.initialAction = Just Initialize
        , M.mountPoint = Nothing
        , M.logLevel = M.DebugAll
        , M.scripts = []
        , M.mailbox = const Nothing
        }

app :: MisoString -> GridComponent
app m_root = mkApp (initialModel m_root)

-- Custom event handler with preventDefault set to True
onClick_ :: a -> Attribute a
onClick_ action = onWithOptions defaultOptions { preventDefault = True } "click" emptyDecoder (const $ const action)


update :: Action -> Effect Model Action
update Initialize = subscribe catalogInTopic OnMessage OnMessageError

update (OnMessage (DisplayItems xs)) = modify $ \m -> (m { display_items = xs })

update (OnMessageError msg) =
    io_ $ consoleError ("CatalogGrid Message decode failure: " <> toMisoString msg)

update (ThreadSelected post) = do
    io_ $ consoleLog $ "ThreadSelected - " <> toMisoString (show post)
    publish catalogOutTopic $ SelectThread post


view :: Model -> View Action
view model =
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ div_
                [ id_ "Grid" ]
                (map (gridItem model) (display_items model))
            ]
        ]

gridItem :: Model -> CatalogPost -> View Action
gridItem m post =
    div_
        [ class_ "thread grid-li grid-size-small" ]
        [ a_
            [ href_ thread_url
            , onClick_ (ThreadSelected post)
            ]
            [ img_
                [ class_ "thread-image"
                , src_ thumb_url
                , title_ ( toMisoString $ show $ CatalogPost.bump_time post )
                ]
            ]
        , div_
            [ class_ "replies" ]
            (
              [ div_
                  [ class_ "meta" ]
                  [ "R: "
                  , b_ [][ text post_count_str ]
                  , "+"
                  ]
              ] ++ (intro subject) ++ body
            )
        ]

  where
    subject :: [ View a ]
    subject = map (text . toMisoString) $ maybeToList $ CatalogPost.subject post

    intro :: [ View a ] -> [ View a ]
    intro [] = []
    intro x = (: []) $ p_
        [ class_ "intro" ]
        [ span_
            [ class_ "subject" ]
            x
        ]

    body :: [ View a ]
    body = map (rawHtml . toMisoString) $ maybeToList $ CatalogPost.body post

    post_count_str :: MisoString
    post_count_str = toMisoString $ (CatalogPost.estimated_post_count post) - 1

    embed_url :: Maybe String
    embed_url =
        (CatalogPost.embed post) >>= Just . (fromRight "") . extractVideoId . T.unpack

    thumb_url :: MisoString
    thumb_url  =
        case embed_url of
            Nothing ->
                case mthumb_path of
                    -- TODO: what about embeds!?
                    Nothing -> "/static/default_thumbnail.png"
                    Just thumb_path -> (media_root m) <> (toMisoString thumb_path)
            Just u -> "https://leftychan.net/vi/" <> toMisoString u <> "/0.jpg"

    mthumb_path :: Maybe Text
    mthumb_path = do
        file_name <- CatalogPost.file_name post
        thumb_ext <- CatalogPost.file_thumb_extension post

        return $
            "/" <> CatalogPost.site_name post
            <> "/" <> CatalogPost.pathpart post
            <> "/" <> (pack $ show $ CatalogPost.board_thread_id post)
            <> "/thumbnail_" <> file_name
            <> "." <> thumb_ext

    thread_url :: MisoString
    thread_url = toMisoString $ T.intercalate "/"
      [ CatalogPost.site_name post
      , CatalogPost.pathpart post
      , pack $ show $ CatalogPost.board_thread_id post
      ]
