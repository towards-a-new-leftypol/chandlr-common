{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.Thread
( Model (..)
, PostWithBody
, initialModel
, Action (..)
, update
, view
, app
, ThreadComponent
, Message (..)
, threadTopic
) where

import Prelude hiding (head)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
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
import Data.List.NonEmpty (head, toList)
import qualified Data.List as L
import Miso.String (toMisoString, MisoString)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime, getCurrentTime)
import Data.Time.Calendar (Day (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)

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

initialModel :: MisoString -> Site -> Model
initialModel m_root s = Model
    { site = s
    , post_bodies = []
    , media_root = m_root
    , current_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
    }

type ThreadComponent parent = Component parent Model Action

data Action
    = OnMessage Message
    | OnMessageError MisoString
    | UpdatePostBodies UTCTime [ PostWithBody ]
    | Initialize
    deriving Eq

data Message = RenderSite MisoString Site
    deriving (Eq, Generic, ToJSON, FromJSON)

threadTopic :: Topic Message
threadTopic = topic "thread"

app :: Model -> ThreadComponent parent
app m = M.Component
    { M.model = m
    , M.initialModel = Nothing
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
    , M.bindings = []
    }


update :: Action -> Effect parent Model Action
update Initialize = subscribe threadTopic OnMessage OnMessageError
update (OnMessage (RenderSite m_root s)) = do
    modify changeModel

    io $ do
        consoleLog "Thread - received RenderSite message"
        let pwbs = Body.getPostWithBodies s
        now <- liftIO $ getCurrentTime
        return $ UpdatePostBodies now pwbs

    where
        changeModel :: Model -> Model
        changeModel Uninitialized = initialModel m_root s
        changeModel m = m { site = s }

update (OnMessageError msg) =
    io_ $ consoleError ("Thread Component Message decode failure: " <> toMisoString msg)

update (UpdatePostBodies t pwbs) = do
    io_ $ consoleLog "Thread - update UpdatePostBodies case"
    modify changeModel

    where
        changeModel :: Model -> Model
        changeModel Uninitialized = Uninitialized
        changeModel m = m { post_bodies = pwbs, current_time = t }


view :: Model -> View model a
view Uninitialized = text ""
view m =
  div_
    []
    [ h1_ [] [ text title ]
    , div_
        [ class_ "thread" ]
        (  (op_post thread_posts)
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

        op_post :: [ Post ] -> [ View model a ]
        op_post [] = [ h2_ [] [ "There's nothing here" ] ]
        op_post (x:_) = op m x backlinks

        title :: MisoString
        title = toMisoString $ (Site.name $ site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


op :: Model -> Post -> Backlinks -> [ View model a ]
op m op_post backlinks =
    [ files_or_embed_view
    , div_
        (
            [ class_ "post op"
            , id_ $ toMisoString $ show $ Post.board_post_id op_post
            ] ++ multi op_post
        )
        [ intro site_ board thread op_post backlinks $ current_time m
        , div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
    ]

    where
        files_or_embed_view :: View model a
        files_or_embed_view =
          case (Post.embed op_post) of
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
        body x = Body.render site_ $ snd $ L.head x


multi :: Post -> [ Attribute a ]
multi post
    | length (Post.attachments post) > 1 = [ class_ "multifile" ]
    | otherwise = []


reply :: Model -> Backlinks -> PostWithBody -> View model a
reply m backlinks (post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        (
            [ class_ "post reply"
            ] ++ multi post
        )
        [ intro site_ board thread post backlinks $ current_time m
        , files_or_embed_view
        , div_
            [ class_ "body" ]
            (Body.render site_ parts)
        ]
    ]

    where
        files_or_embed_view :: View model a
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

