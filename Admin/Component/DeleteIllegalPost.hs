{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Admin.Component.DeleteIllegalPost where

import Miso
    ( topic
    , Topic
    , defaultEvents
    , Effect
    , consoleLog
    , MisoString
    , subscribe
    , io_
    , consoleError
    , toMisoString
    , text
    , View
    , modify
    )

import qualified Miso as M
import Miso.Html.Property (class_)
import Miso.Html.Element
    ( div_
    , p_
    , span_
    )
import Miso.CSS
    ( style_
    , display
    )
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (isJust)
import qualified Data.Map as Map

import qualified Common.Component.Modal as Modal
import qualified Common.Component.Thread.Model as T
import qualified Common.Network.PostType as P
import Common.Component.PostViews (op, reply)
#ifdef FRONT_END
import JSFFI.Saddle
    ( freezeBodyScrolling
    , unFreezeBodyScrolling
    )
#endif

newtype Model = Model
    { threadData :: Maybe T.Model -- some may say this is lazy... really it's peak
    }
    deriving Eq

data Action
    = Initialize
    | OnMessageIn InMessage
    | OnErrorMessage MisoString
    | Cancel

newtype InMessage = InMessage T.Model
    deriving (Generic, ToJSON, FromJSON)

deleteIllegalPostInTopic :: Topic InMessage
deleteIllegalPostInTopic = topic "deleteIllegal-in"

type DeleteIllegalPostComponent parent = M.Component parent Model Action

initialModel :: Model
initialModel = Model Nothing

app :: DeleteIllegalPostComponent parent
app = M.Component
    { M.model = initialModel
    , M.hydrateModel = Nothing
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
#ifdef FRONT_END
update Initialize = do
    io_ $ consoleLog "DeleteIllegalPostComponent Init"
    subscribe deleteIllegalPostInTopic OnMessageIn OnErrorMessage

update (OnMessageIn (InMessage x)) = do
    io_ $ consoleLog "DeleteIllegalPostComponent received a message!"
    modify (\m -> m { threadData = Just x } )
    io_ freezeBodyScrolling

update (OnErrorMessage e) = io_ $ consoleError e

update Cancel = do
    modify (\m -> m { threadData = Nothing } )
    io_ unFreezeBodyScrolling
#else
update = undefined
#endif

view :: Model -> View model Action
view m = div_ hide render

    where
        hide
            | isJust $ threadData m = [ class_ "modal-dialog" ]
            | otherwise = [ style_ [ display "none" ] ]

        render =
            case threadData m of
                Nothing -> []
                Just x ->
                    [ Modal.view
                        ( Modal.Model
                            { Modal.cancel = Cancel
                            , Modal.submit = undefined
                            , Modal.content = content x { T.admin = False }
                            , Modal.title = "Delete post and attachments?"
                            , Modal.action = "Delete"
                            }
                        )
                    ]

        content :: T.Model -> View model Action
        content threadModel@T.Model { T.post_bodies = (pwb@(post, _):_) } =
            div_ [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "warning-message" ]
                    [ div_ [ class_ "warning-icon" ] [ "⚠" ]
                    , p_ []
                        [ "This will delete "
                        , a
                        , span_
                            [ class_ "warning-message__strong-word" ]
                            [ "No."
                            , text $ toMisoString $ show $ P.board_post_id post
                            ]
                        , " as well as any other post containing the same attachments as this post:"
                        ]
                    ]
                , div_ [ class_ "modal-dialog__post-preview" ]
                    (
                        if isOp
                        then
                            op (const []) threadModel post Map.empty
                            ++ [ div_ [ class_ "clearfix" ] [] ]
                        else
                            [ reply (const []) threadModel Map.empty pwb ]
                    )
                ]

            where
                isOp :: Bool
                isOp = P.local_idx post == 1

                a
                    | isOp = "thread "
                    | otherwise = "post "

        content T.Model { T.post_bodies = [] } =
            div_
                [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "warning-message" ]
                    [ p_ [] [ "Invalid state - no post information!" ]
                    ]
                ]
