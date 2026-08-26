{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Component.Catalog where

import Miso
    ( Component (mount, onPropsChanged, mailbox, hydrateModel)
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
    , checkMail
    )

import Miso.JSON (Value)
import Data.Time.Clock (UTCTime)
import qualified Common.Component.CatalogGrid as Grid
import qualified Data.Sequence as Seq
import Data.Sequence ((|>), ViewR ((:>)))
import Control.Monad (when, unless)
import Data.IORef (readIORef)

import qualified Common.Network.CatalogPostType as C
import Common.FrontEnd.Types
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils
import qualified Common.Network.BoardType as Board
import Common.Component.InfiniteScroll.Action hiding (Action)

pattern FetchCatalogBottom :: Client.ReturnTopicName
pattern FetchCatalogBottom = "fetch-catalog-bottom"

type CatalogPages = Pages Seq.Seq C.CatalogPost

data Model = Model
    { pages :: CatalogPages
    , scrollTime :: Maybe (UTCTime, Integer)
    } deriving Eq

emptyPages :: CatalogPages
emptyPages = Pages Seq.empty

isEmptyPages :: Pages b c -> Bool
isEmptyPages (Pages a) = Seq.null a

initialModel :: Model
initialModel = Model
    { pages = emptyPages
    , scrollTime = Nothing
    }

data Props = Props
    { mediaRoot :: MisoString
    , currentTime :: Time
    , selectedBoards :: Maybe [ Board.Board ]
    , fetchCount :: Int
    }
    deriving Eq

data Action
    = Initialize
    | ClientResponse Client.ReturnTopicName Client.MessageOut
    | OnErrorMessage MisoString
    | PropsChanged
    | OnScrollMessage InfScrollOutMsg

app :: Eq context => InitCtxRef -> Component context Props Model Action
app ctxRef = (component initialModel update view)
    { mount = Just Initialize
    , onPropsChanged = Just $ const $ const PropsChanged
    , mailbox = handleMail
    , hydrateModel  = Just $ initializeModel ctxRef
    }

    where
        handleMail :: Value -> Maybe Action
        handleMail = checkMail OnScrollMessage OnErrorMessage

initializeModel :: InitCtxRef -> IO Model
initializeModel ctxRef = do
    putStrLn "MainComponent initializeModel"
    ctx <- readIORef ctxRef
    let initialPayload = init_payload ctx
        initialData_ = initialData initialPayload

    return $ initialModel { pages = pagesFromInitialData initialData_ }

    where
        pagesFromInitialData :: InitialData -> CatalogPages
        pagesFromInitialData (CatalogData posts) = Pages $ Seq.singleton $
            Page $ Seq.fromList posts
        pagesFromInitialData _ = emptyPages


view :: Eq context => context -> Props -> Model -> View context Action
view _ props model = vfrag [ mountWithProps (mkGridProps model props) Grid.app ]

mkGridProps :: Model -> Props -> Grid.Props (Pages Seq.Seq)
mkGridProps m p = Grid.Props (pages m) (mediaRoot p)

clientFetchCatalogBottom :: Topic Client.MessageOut
clientFetchCatalogBottom = topic FetchCatalogBottom

update :: Action -> Effect context Props Model Action
update Initialize = do
    subscribe clientFetchCatalogBottom (ClientResponse FetchCatalogBottom) OnErrorMessage

    model <- get

    io_ $
        consoleLog $ "Catalog Initialize. isEmptyPages: " <> toMisoString (show (isEmptyPages $ pages model))

    when (isEmptyPages $ pages model) $ issue PropsChanged

update PropsChanged = do
    props <- getProps
    model <- get
    io_ $ do
        consoleLog "Catalog - PropsChanged, asking client for latest catalog"
        publish Client.clientInTopic
            ( FetchCatalogBottom
            , Client.FetchLatest $ Client.FetchCatalogArgs
                { Client.selected_time = (utcTimeFromTime $ currentTime props)
                , Client.board_ids =
                    (map Board.board_id <$> selectedBoards props)
                , Client.scroll_time = fst <$> scrollTime model
                , Client.scroll_thread_id = snd <$> scrollTime model
                , Client.thread_count = fetchCount props
                }
            )

    where
        utcTimeFromTime :: Time -> UTCTime
        utcTimeFromTime (Now t) = t
        utcTimeFromTime (Then t) = t

update (ClientResponse FetchCatalogBottom (Client.ReturnResult result)) = do
    io_ $ consoleLog "ClientResponse - have Catalog encoded result"
    Utils.helper result $
        \catalogPosts -> do
            io_ $ consoleLog $ "ClientResponse - Catalog, saving catalog posts as Pages. number of posts: " <> toMisoString (show $ length catalogPosts)
            modify ( \m -> m { pages = addPage (pages m) catalogPosts } )

    where
        addPage :: CatalogPages -> [ C.CatalogPost ] -> CatalogPages
        addPage (Pages p) posts
            | Seq.null p = Pages $ Seq.singleton $
                Page $ Seq.fromList posts
            | otherwise = Pages $ p |> Page (Seq.fromList posts)

update (ClientResponse _ _) = error "Catalog error - unexpected Client response topic"

update (OnScrollMessage (Grow Bottom)) = do
    model <- get

    unless (isEmptyPages (pages model)) $ do
        modify $ \m -> m { scrollTime = scrollKey (pages m) }
        io_ $ consoleLog "Catalog Scroll Message Grow Bottom"
        issue PropsChanged

    where
        scrollKey :: CatalogPages -> Maybe (UTCTime, Integer)
        scrollKey = fmap (\post -> (C.bump_time post, C.thread_id post)) . getLast

update (OnScrollMessage _) = io_ $ consoleLog "Catalog UNIMPLEMENTED Scroll Message"

update (OnErrorMessage msg) =
    io_ $ consoleError ("Catalog Component OnErrorMessage decode failure: " <> toMisoString msg)


-- | Safely gets the last element of a Seq
lastOf :: Seq.Seq a -> Maybe a
lastOf s = case Seq.viewr s of
    Seq.EmptyR -> Nothing
    _ :> x -> Just x

-- | Gets the last post from the last page
getLast :: CatalogPages -> Maybe C.CatalogPost
getLast (Pages ps) = lastOf ps >>= lastOf . pageRows
