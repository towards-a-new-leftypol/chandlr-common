{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Common.Component.Thread
( Model (..)
, PostWithBody
, Action (..)
, update
, view
, app
, ThreadComponent
, Message (..)
, threadTopic
) where

import Prelude hiding (head)
import Miso
  ( View
  , Effect
  , text
  , Attribute
  , Component
  , defaultEvents
  , io
  , io_
  , Topic
  , topic
  , consoleError
  , consoleLog
  , subscribe
  , publish
  )
import Miso.Html
  ( div_
  , h1_
  , h2_
  )
import Miso.Html.Property
  ( class_
  , id_
  )
import qualified Miso as M
import Miso.Binding ((-->))
import Data.List.NonEmpty (head, toList)
import qualified Data.List as L
import Miso.String (toMisoString, MisoString)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.IORef (readIORef)

import Common.Network.SiteType (Site)
import qualified Common.Network.SiteType as Site
import Common.Network.PostType (Post)
import qualified Common.Network.PostType as Post
import qualified Common.Network.BoardType as Board
import Common.Network.BoardType (Board)
import qualified Common.Network.ThreadType as Thread
import Common.Network.ThreadType (Thread)
import Common.Component.Thread.Files (files)
import Common.Component.Thread.Intro (intro)
import Common.Component.Thread.Embed (embed)
import Common.Component.Thread.Model
import Common.Parsing.BodyParser
import qualified Common.Component.BodyRender as Body
import Common.Component.Thread.Types
import qualified Common.FrontEnd.JSONSettings as Settings
import Common.FrontEnd.Types
import Common.Admin.DeleteBtn (deleteBtn)
import qualified Common.FrontEnd.Model as FE
import qualified Common.Admin.Component.DeleteIllegalPost as DIP

type ThreadComponent parent = Component parent Model Action

threadTopic :: Topic Message
threadTopic = topic "thread"

app :: InitCtxRef -> ThreadComponent FE.Model
app ctxRef = M.Component
    { M.model = emptyModel
    , M.hydrateModel = Just $ initializeModel ctxRef
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.bindings =
        [ FE.getSetAdmin --> getSetAdmin
        , FE.getSetMediaRoot --> getSetMediaRoot
        ]
    }

initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
  ctx <- readIORef ctxRef

  let settings = init_settings ctx
  let initialPayload = init_payload ctx

  case initialPayload of
    (InitialDataPayload t (ThreadData s pwbs)) ->
          return Model
            { site = s
            , media_root = Settings.media_root settings
            , post_bodies = pwbs
            , current_time = t
            , admin = Settings.admin settings
            }

    _ -> return emptyModel


update :: Action -> Effect parent Model Action
update Initialize = subscribe threadTopic OnMessage OnMessageError
update (OnMessage (RenderSite _ s)) = do
    modify (\m -> m { site = s })

    io $ do
        consoleLog "Thread - received RenderSite message"
        let pwbs = Body.getPostWithBodies s
        now <- liftIO $ getCurrentTime
        return $ UpdatePostBodies now pwbs

update (OnMessageError msg) =
    io_ $ consoleError ("Thread Component Message decode failure: " <> toMisoString msg)

update (UpdatePostBodies t pwbs) = do
    io_ $ consoleLog "Thread - update UpdatePostBodies case"
    modify (\m -> m { post_bodies = pwbs, current_time = t })

update (OnDeleteBtn pwb) = do
    io_ $ consoleLog "OnDeleteBtn"
    publish DIP.deleteIllegalPostInTopic $ DIP.InMessage pwb


view :: Model -> View model Action
view m =
  div_
    []
    [ h1_ [] [ text title ]
    , div_
        [ class_ "thread" ]
        (  op_post thread_posts
        ++ map (reply m backlinks) (drop 1 (post_bodies m))
        )
    ]

    where
        thread_posts :: [ Post ]
        thread_posts =
            concatMap (toList . Thread.posts) $
                concatMap (toList . Board.threads) $
                    Site.boards (site m)

        backlinks :: Backlinks
        backlinks = collectBacklinks (post_bodies m)

        op_post :: [ Post ] -> [ View model Action ]
        op_post [] = [ h2_ [] [ "There's nothing here" ] ]
        op_post (x:_) = op m x backlinks

        title :: MisoString
        title = toMisoString $ Site.name (site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


op :: Model -> Post -> Backlinks -> [ View model Action ]
op m op_post backlinks =
    [ div_
        (
            [ class_ "post op"
            , id_ $ toMisoString $ show $ Post.board_post_id op_post
            ] ++ multi op_post
        )
        ( intro site_ board thread op_post backlinks (current_time m)
        : files_or_embed_view
        : deleteBtn_ m (L.head $ post_bodies m)
        ++
        [ div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
        )
    ]

    where
        files_or_embed_view :: View model a
        files_or_embed_view =
          case Post.embed op_post of
            Just _ -> embed op_post
            Nothing -> files (media_root m) site_ op_post


        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board

        body :: [ PostWithBody ] -> [ View model a ]
        body [] = []
        body (x:_) = Body.render site_ $ snd x


multi :: Post -> [ Attribute a ]
multi post
    | length (Post.attachments post) > 1 = [ class_ "multifile" ]
    | otherwise = []


reply :: Model -> Backlinks -> PostWithBody -> View model Action
reply m backlinks pwb@(post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        ( class_ "post reply" : multi post )
        ( intro site_ board thread post backlinks (current_time m)
        : deleteBtn_ m pwb
        ++
        [ files_or_embed_view
        , div_
            [ class_ "body" ]
            (Body.render site_ parts)
        ]
        )
    ]

    where
        files_or_embed_view :: View model Action
        files_or_embed_view =
          case (Post.embed post) of
            Just _ -> embed post
            Nothing -> files (media_root m) site_ post

        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board


deleteBtn_ :: Model -> PostWithBody -> [ View model Action ]
deleteBtn_ m p
  | admin m = [ deleteBtn (OnDeleteBtn p) ]
  | otherwise = []
