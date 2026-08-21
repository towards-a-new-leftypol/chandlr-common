{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Component.Catalog where

import Miso
    ( Component (mount, onPropsChanged)
    , component
    , vfrag
    , View
    , mountWithProps
    , MisoString
    , Effect
    , subscribe
    , Topic
    , topic
    , modify
    , io_
    , consoleLog
    , toMisoString
    , getProps
    , consoleError
    , get
    , publish
    , io_
    , issue
    )

import Data.Time.Clock (UTCTime)
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import qualified Data.Sequence as Seq
import Control.Monad (when)

import Common.Network.CatalogPostType (CatalogPost)
import Common.FrontEnd.Types (Pages (..), Page (..), Time (..))
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils
import qualified Common.Network.BoardType as Board

pattern SenderLatest :: Client.ReturnTopicName
pattern SenderLatest = "main-latest"

type CatalogPages = Pages [] CatalogPost

data Model = Model
    { pages :: CatalogPages
    , scrollTime :: Maybe UTCTime
    } deriving Eq

initialModel :: Model
initialModel = Model
    { pages = Pages Seq.empty
    , scrollTime = Nothing
    }

data Props = Props
    { mediaRoot :: MisoString
    , currentTime :: Time
    , selectedBoards :: Maybe [ Board.Board ]
    }
    deriving Eq

data Action
    = Initialize
    | ClientResponse Client.MessageOut
    | OnErrorMessage MisoString
    | PropsChanged

app :: Eq context => Component context Props Model Action
app = (component initialModel update view)
    { mount = Just Initialize
    , onPropsChanged = Just $ const $ const PropsChanged
    }

view :: Eq context => context -> Props -> Model -> View context Action
view _ props model = vfrag [ mountWithProps (mkGridProps model props) Grid.app ]

mkGridProps :: Model -> Props -> Grid.Props (Pages [])
mkGridProps m p = Grid.Props (pages m) (mediaRoot p)

clientLatestReturnTopic :: Topic Client.MessageOut
clientLatestReturnTopic = topic SenderLatest

update :: Action -> Effect context Props Model Action
update Initialize = do
    subscribe clientLatestReturnTopic ClientResponse OnErrorMessage

    model <- get

    io_ $
        consoleLog $ "Catalog Initialize. emptyPages: " <> toMisoString (show (emptyPages $ pages model))

    when (emptyPages $ pages model) $ issue PropsChanged

    where
        emptyPages :: Pages b c -> Bool
        emptyPages (Pages a) = Seq.null a

update PropsChanged = do
    props <- getProps
    io_ $ do
        consoleLog "Catalog - PropsChanged, asking client for latest catalog"
        publish Client.clientInTopic
            ( SenderLatest
            , Client.FetchLatest
                (utcTimeFromTime $ currentTime props)
                (map Board.board_id <$> selectedBoards props)
            )

    where
        utcTimeFromTime :: Time -> UTCTime
        utcTimeFromTime (Now t) = t
        utcTimeFromTime (Then t) = t

update (ClientResponse (Client.ReturnResult result)) = do
    io_ $ consoleLog "ClientResponse - have Catalog encoded result"
    Utils.helper result $
        \catalogPosts -> do
            io_ $ consoleLog $ "ClientResponse - Catalog, saving catalog posts as Pages. number of posts: " <> toMisoString (show $ length catalogPosts)
            modify
                ( \m -> m
                    { pages = Pages $ Seq.singleton $ Page catalogPosts }
                )

update (OnErrorMessage msg) =
    io_ $ consoleError ("Catalog Component OnErrorMessage decode failure: " <> toMisoString msg)
