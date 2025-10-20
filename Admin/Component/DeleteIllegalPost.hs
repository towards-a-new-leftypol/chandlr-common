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
    , View
    )

import qualified Miso as M
import Miso.Html.Element
    ( div_
    )
import Miso.CSS
    ( style_
    , display
    )
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.State (modify)

import Common.Component.Thread.Model (PostWithBody)

data Model = Model
    { postWithBody :: Maybe PostWithBody
    , displayModal :: Bool
    }
    deriving Eq

data Action
    = Initialize
    | OnMessageIn InMessage
    | OnErrorMessage MisoString

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

update (OnErrorMessage e) = io_ $ consoleError e

view :: Model -> View model Action
view m = div_ hide []
    where
        hide
            | displayModal m = []
            | otherwise = [ style_ [ display "none" ] ]
