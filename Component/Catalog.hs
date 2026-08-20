{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Component.Catalog where

import Miso
    ( Component (mount)
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
    )

import Data.Time.Clock (UTCTime)
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import qualified Data.Sequence as Seq

import Common.Network.CatalogPostType (CatalogPost)
import Common.FrontEnd.Types (Pages (..), Page (..), Time (..))
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils
import qualified Common.Network.BoardType as Board
import Control.Monad (when)

pattern SenderLatest :: Client.ReturnTopicName
pattern SenderLatest = "main-latest"

type CatalogPages = Pages (UTCTime, Maybe UTCTime) [] CatalogPost

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

app :: Eq context => Component context Props Model Action
app = (component initialModel update view)
    { mount = Just Initialize }

view :: Eq context => context -> Props -> Model -> View context Action
view _ props model = vfrag [ mountWithProps (mkGridProps model props) Grid.app ]

mkGridProps :: Model -> Props -> Grid.Props (Pages (UTCTime, Maybe UTCTime) [])
mkGridProps m p = Grid.Props (pages m) (mediaRoot p)

clientLatestReturnTopic :: Topic Client.MessageOut
clientLatestReturnTopic = topic SenderLatest

update :: Action -> Effect context Props Model Action
update Initialize = do
    subscribe clientLatestReturnTopic ClientResponse OnErrorMessage

    model <- get

    io_ $
        consoleLog $ "Catalog Initialize. emptyPages: " <> toMisoString (show (emptyPages $ pages model))

    when (emptyPages $ pages model) $ do
        props <- getProps
        io_ $ do
            consoleLog "Catalog - pages are empty, asking client for latest catalog"
            publish Client.clientInTopic
                ( SenderLatest
                , Client.FetchLatest
                    (utcTimeFromTime $ currentTime props)
                    (map Board.board_id <$> selectedBoards props)
                )

    where
        emptyPages :: Pages a b c -> Bool
        emptyPages (Pages a) = Seq.null a

        utcTimeFromTime :: Time -> UTCTime
        utcTimeFromTime (Now t) = t
        utcTimeFromTime (Then t) = t

update (ClientResponse (Client.ReturnResult result)) = do
    io_ $ consoleLog "ClientResponse - have Catalog encoded result"
    Utils.helper result $
        \catalogPosts -> do
            io_ $ consoleLog $ "ClientResponse - Catalog, saving catalog posts as Pages. number of posts: " <> toMisoString (show $ length catalogPosts)
            props <- getProps
            modify
                ( \m -> m
                    { pages = Pages $ Seq.singleton $
                        Page
                            ( utcTimeFromTime $ currentTime props
                            , scrollTime m
                            )
                            catalogPosts
                    }
                )

    where
        utcTimeFromTime :: Time -> UTCTime
        utcTimeFromTime (Then t) = t
        utcTimeFromTime (Now t) = t

update (OnErrorMessage msg) =
    io_ $ consoleError ("Catalog Component OnErrorMessage decode failure: " <> toMisoString msg)
