{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Common.Component.CatalogGrid
( Model (..)
, Action (..)
, view
, update
, app
, initialItems
, GridComponent
, OutMessage (..)
, catalogOutTopic
) where

import Data.Maybe (maybeToList)
import Data.Either (fromRight)
import Miso
    ( View
    , Effect
    , text
    -- , rawHtml
    , onWithOptions
    , defaultOptions
    , Options (_preventDefault)
    , Attribute
    , emptyDecoder
    , publish
    , io_
    , consoleLog
    )
import Miso.Html
    ( div_
    , img_
    , a_
    , b_
    , span_
    , p_
    )
import Miso.Html.Property
    ( src_
    , title_
    , id_
    , class_
    , href_
    )
import Miso.String (toMisoString, fromMisoString, MisoString)
import qualified Data.JSString as JStr
import qualified Miso as M
import Miso.Binding ((-->))
import Data.IORef (readIORef)
#ifdef FRONT_END
import Control.Monad.IO.Class (liftIO)
import Miso (JSM)
#endif

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import Common.Parsing.EmbedParser (extractVideoId)
import Common.Component.CatalogGrid.GridTypes
import qualified Common.Network.SiteType as Site
import qualified Common.Component.BodyRender as Body
import Common.FrontEnd.Types
import qualified Common.FrontEnd.Model as FE
import qualified Common.FrontEnd.JSONSettings  as Settings

import Debug.Trace (trace)

app :: InitCtxRef -> GridComponent FE.Model
app ctxRef =
    M.Component
        { M.model = Model [] ""
        , M.hydrateModel = Just $ initializeModel ctxRef
        , M.update = update
        , M.view = view
        , M.subs = []
        , M.events = M.defaultEvents
        , M.styles = []
        , M.initialAction = Nothing
        , M.mountPoint = Nothing
        , M.logLevel = M.DebugAll
        , M.scripts = []
        , M.mailbox = const Nothing
        , M.bindings = [ FE.getSetCatalogPosts --> getSetDisplayItems ]
        }


#ifdef FRONT_END
initializeModel :: InitCtxRef -> JSM Model
initializeModel ctxRef = liftIO $ do
#else
initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
#endif
    putStrLn "CatalogGrid initializeModel"
    ctx <- readIORef ctxRef

    let asdf = initialItems $ initialData $ init_payload ctx
    putStrLn $ "CatalogGrid initializeModel item size: " ++ (show $ length asdf)

    return $ Model
        (initialItems $ initialData $ init_payload ctx)
        (toMisoString $ Settings.media_root $ init_settings ctx)


initialItems :: InitialData -> [ CatalogPost ]
initialItems (CatalogData catalog_posts) = catalog_posts
initialItems (SearchData catalog_posts) = catalog_posts
initialItems _ = []


-- Custom event handler with preventDefault set to True
onClick_ :: a -> Attribute a
onClick_ action = onWithOptions defaultOptions { _preventDefault = True } "click" emptyDecoder (const $ const action)


update :: Action -> Effect parent Model Action
update (ThreadSelected post) = do
    io_ $ consoleLog $ "ThreadSelected - " <> toMisoString (CatalogPost.thread_id post)
    publish catalogOutTopic $ SelectThread post


view :: Model -> View model Action
view model =
    trace ("CatalogGrid view being called. Number of catalog items in model: " <> (show $ length $ display_items model)) $
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ div_
                [ id_ "Grid" ]
                (map (gridItem model) (display_items model))
            ]
        ]

gridItem :: Model -> CatalogPost -> View model Action
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
    subject :: [ View model a ]
    subject = map (text . toMisoString) $ maybeToList $ CatalogPost.subject post

    intro :: [ View model a ] -> [ View model a ]
    intro [] = []
    intro x = (: []) $ p_
        [ class_ "intro" ]
        [ span_
            [ class_ "subject" ]
            x
        ]

    body :: [ View model a ]
    -- body = map (rawHtml . toMisoString) $ maybeToList $ CatalogPost.body post
    body =
        let site = Site.fromCatalogPost post
        in
            concatMap ((Body.render site) . snd) (Body.getPostWithBodies site)

    post_count_str :: MisoString
    post_count_str = toMisoString $ (CatalogPost.estimated_post_count post) - 1

    embed_url :: Maybe String
    embed_url =
        (CatalogPost.embed post) >>= Just . (fromRight "") . extractVideoId . fromMisoString

    thumb_url :: MisoString
    thumb_url  =
        case embed_url of
            Nothing ->
                case mthumb_path of
                    -- TODO: what about embeds!?
                    Nothing -> "/static/default_thumbnail.png"
                    Just thumb_path -> (media_root m) <> (toMisoString thumb_path)
            Just u -> "https://leftychan.net/vi/" <> toMisoString u <> "/0.jpg"

    mthumb_path :: Maybe MisoString
    mthumb_path = do
        file_name <- CatalogPost.file_name post
        thumb_ext <- CatalogPost.file_thumb_extension post

        return $
            "/" <> CatalogPost.site_name post
            <> "/" <> CatalogPost.pathpart post
            <> "/" <> (toMisoString $ show $ CatalogPost.board_thread_id post)
            <> "/thumbnail_" <> file_name
            <> "." <> thumb_ext

    thread_url :: MisoString
    thread_url = toMisoString $ JStr.intercalate "/" $ map fromMisoString $
      [ CatalogPost.site_name post
      , CatalogPost.pathpart post
      , toMisoString $ show $ CatalogPost.board_thread_id post
      ]
