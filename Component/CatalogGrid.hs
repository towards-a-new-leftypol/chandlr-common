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
    , Component
    )
import Miso.String (toMisoString, MisoString)
import qualified Miso as M

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Parsing.EmbedParser (extractVideoId)
import Common.FrontEnd.Action (mkGetThread)
import qualified Common.FrontEnd.MainComponent as MC

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

type GridComponent = Component "catalog-grid" Model Action

initialModel :: MisoString -> Model
initialModel media_root_ = Model
    { display_items = []
    , media_root = toMisoString media_root_
    }

data Action
    = DisplayItems [ CatalogPost ]
    | ThreadSelected CatalogPost


data Interface a = Interface
    { passAction :: Action -> a -- We're not using this.
    , threadSelected :: CatalogPost -> a
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
update _ (DisplayItems xs) = modify $ \m -> (m { display_items = xs })
update mc (ThreadSelected post) = io_ $ notify mc $ mkGetThread post


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
