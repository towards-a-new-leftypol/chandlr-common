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

type Model = ()
data Action
    = Initialize
    | OnMessageIn InMessage
    | OnErrorMessage MisoString

data InMessage = InMessage
    deriving (Generic, ToJSON, FromJSON)

deleteIllegalPostInTopic :: Topic InMessage
deleteIllegalPostInTopic = topic "deleteIllegal-in"

type DeleteIllegalPostComponent parent = M.Component parent Model Action


app :: DeleteIllegalPostComponent parent
app = M.Component
    { M.model = ()
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

update (OnMessageIn _) = io_ $ consoleLog "DeleteIllegalPostComponent received a message!"
update (OnErrorMessage e) = io_ $ consoleError e

view :: Model -> View model Action
view _ = div_ [ style_ [ display "none" ] ] []
