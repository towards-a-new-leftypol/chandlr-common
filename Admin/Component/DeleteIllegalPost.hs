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
import Control.Monad.State (modify)

import Common.Component.Thread.Model (PostWithBody)
import qualified Common.Component.Modal as Modal
import qualified Common.Network.PostType as P
import JSFFI.Saddle
    ( freezeBodyScrolling
    , unFreezeBodyScrolling
    )

data Model = Model
    { postWithBody :: Maybe PostWithBody
    , displayModal :: Bool
    }
    deriving Eq

data Action
    = Initialize
    | OnMessageIn InMessage
    | OnErrorMessage MisoString
    | Cancel

newtype InMessage = InMessage PostWithBody
    deriving (Generic, ToJSON, FromJSON)

deleteIllegalPostInTopic :: Topic InMessage
deleteIllegalPostInTopic = topic "deleteIllegal-in"

type DeleteIllegalPostComponent parent = M.Component parent Model Action

initialModel :: Model
initialModel = Model Nothing False


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
update Initialize = do
    io_ $ consoleLog "DeleteIllegalPostComponent Init"
    subscribe deleteIllegalPostInTopic OnMessageIn OnErrorMessage

update (OnMessageIn (InMessage pwb)) = do
    io_ $ consoleLog "DeleteIllegalPostComponent received a message!"
    modify (\m -> m { displayModal = True, postWithBody = Just pwb } )
    io_ freezeBodyScrolling

update (OnErrorMessage e) = io_ $ consoleError e

update Cancel = do
    modify (\m -> m { displayModal = False, postWithBody = Nothing } )
    io_ unFreezeBodyScrolling

view :: Model -> View model Action
view m = div_ hide render

    where
        hide
            | displayModal m = [ class_ "modal-dialog" ]
            | otherwise = [ style_ [ display "none" ] ]

        render
            | displayModal m =
                [ Modal.view
                    ( Modal.Model
                        { Modal.cancel = Cancel
                        , Modal.submit = undefined
                        , Modal.content = content
                        , Modal.title = "Delete Post and Attachments?"
                        , Modal.action = "Delete"
                        }
                    )
                ]
            | otherwise = []

        content =
            case postWithBody m of
                Nothing ->
                    Modal.view
                        ( Modal.Model
                            { Modal.cancel = Cancel
                            , Modal.submit = Cancel
                            , Modal.content =
                                div_
                                    [ class_ "modal-dialog__content" ]
                                    [ p_ [] [ "Nothing was selected to delete!" ]
                                    ]
                            , Modal.title = "Unexpected state!"
                            , Modal.action = "Cancel"
                            }
                        )
                Just pwb -> displayWarning pwb

        displayWarning :: PostWithBody -> View model Action
        displayWarning (post, postParts) =
            div_ [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "warning-message" ]
                    [ div_ [ class_ "warning-icon" ] []
                    , "This will delete "
                    , a
                    , span_
                        [ class_ "warning__strong-word" ]
                        [ "No."
                        , text $ toMisoString $ show $ P.board_post_id post
                        ]
                    ]
                    , "as well as any other post containing the same attachments as this post."
                ]

            where
                op :: Bool
                op = P.local_idx post == 1

                a
                    | op = "thread"
                    | otherwise = "post"
