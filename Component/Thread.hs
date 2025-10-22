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
  , get
  )
import Miso.Html
  ( div_
  , h1_
  , h2_
  )
import Miso.Html.Property
  ( class_
  )
import qualified Miso as M
import Miso.Binding ((-->))
import Data.List.NonEmpty (head, toList)
import Miso.String (toMisoString, MisoString)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)
import Data.IORef (readIORef)

import qualified Common.Network.SiteType as Site
import Common.Network.PostType (Post)
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import Common.Component.Thread.Model
import Common.Parsing.BodyParser
import qualified Common.Component.BodyRender as Body
import Common.Component.Thread.Types
import qualified Common.FrontEnd.JSONSettings as Settings
import Common.FrontEnd.Types
import qualified Common.FrontEnd.Model as FE
import qualified Common.Admin.Component.DeleteIllegalPost as DIP
import Common.Admin.DeleteBtn (deleteBtn)
import Common.Component.PostViews (op, reply)

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
    model <- get
    publish DIP.deleteIllegalPostInTopic $ DIP.InMessage model { post_bodies = [ pwb ] }


view :: Model -> View model Action
view m =
  div_
    []
    [ h1_ [] [ text title ]
    , div_
        [ class_ "thread" ]
        (  op_post thread_posts
        ++ map (reply (deleteBtn_ m) m backlinks) (drop 1 (post_bodies m))
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
        op_post (x:_) = op (deleteBtn_ m) m x backlinks

        title :: MisoString
        title = toMisoString $ Site.name (site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


deleteBtn_
    :: Model
    -> PostWithBody
    -> [ View model Action ]
deleteBtn_ m p
  | admin m = [ deleteBtn (OnDeleteBtn p) ]
  | otherwise = []
