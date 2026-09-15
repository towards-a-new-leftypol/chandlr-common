{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Common.Component.Catalog where

import Miso
    ( Component (mount, onPropsChanged, mailbox, hydrateModel)
    , component
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
    , put
    , mailParent
    , DOMRef
    )

import Miso.Html (div_)
import Miso.Html.Property (id_, class_)
import Miso.Event (onCreatedWith)
import Miso.JSON (Value)
import Data.Time.Clock (UTCTime)
import qualified Common.Component.CatalogGrid as Grid
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL (..), ViewR (..), viewr, viewl)
import Control.Monad (when, unless)
import Data.IORef (readIORef)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import qualified Common.Network.CatalogPostType as C
import Common.FrontEnd.Types
import qualified Common.Network.ClientTypes as Client
import qualified Common.Utils as Utils
import qualified Common.Network.BoardType as Board
import Common.Component.InfiniteScroll.Action hiding (Action (..))

pattern FetchCatalogBottom :: Client.ReturnTopicName
pattern FetchCatalogBottom = "fetch-catalog-bottom"

type CatalogPages = Seq (Seq C.CatalogPost)

data Model = Model
    { pages :: CatalogPages
    , scrollTime :: Maybe (UTCTime, Integer)
    , pageHeightPixels :: Map.Map Integer Int -- map post_id to height
    } deriving Eq

initialModel :: Model
initialModel = Model
    { pages = Seq.empty
    , scrollTime = Nothing
    , pageHeightPixels = Map.empty
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
    | NextPage
    | OnScrollMessage InfScrollOutMsg
    | NewPageInDom Integer DOMRef

app :: Eq context => InitCtxRef -> Component context Props Model Action
app ctxRef = (component initialModel update view)
    { mount = Just Initialize
    , onPropsChanged = Just $ const $ const PropsChanged
    , mailbox = handleMail
    , hydrateModel  = Just $ initializeModel ctxRef
    }


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
        pagesFromInitialData (CatalogData posts) = Seq.singleton $
            Seq.fromList posts
        pagesFromInitialData _ = Seq.empty


view :: Eq context => context -> Props -> Model -> View context Action
view _ props model = div_
    [ id_ "Grid" ]
    (foldMap ((: []) . pageView) (pages model))

    where
        pageView page = div_
            [ class_ "grid-page"
            , onCreatedWith $ NewPageInDom $ C.post_id $ fromJust $ firstOf page
            ]
            [ mountWithProps (mkGridProps page props) Grid.app ]


mkGridProps :: Seq.Seq C.CatalogPost -> Props -> Grid.Props Seq
mkGridProps page p = Grid.Props page (mediaRoot p)


clientFetchCatalogBottom :: Topic Client.MessageOut
clientFetchCatalogBottom = topic FetchCatalogBottom


update :: Action -> Effect context Props Model Action
update Initialize = do
    subscribe clientFetchCatalogBottom (ClientResponse FetchCatalogBottom) OnErrorMessage

    model <- get

    io_ $
        consoleLog $ "Catalog Initialize. isEmptyPages: " <> toMisoString (show (Seq.null $ pages model))

    when (Seq.null $ pages model) $ issue PropsChanged

update PropsChanged = do
    put initialModel
    mailParent Reset
    issue NextPage

update NextPage = do
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
            let posts = Seq.fromList catalogPosts
            modify ( \m -> m { pages = addPage (pages m) posts } )

            if Seq.length posts == 0
            then
                mailParent (Exhausted Bottom)
            else
                mailParent (Loaded Bottom)

    where
        addPage :: CatalogPages -> Seq C.CatalogPost -> CatalogPages
        addPage ps posts
            | Seq.null ps = Seq.singleton posts
            | otherwise = ps |> posts

update (ClientResponse _ _) = error "Catalog error - unexpected Client response topic"

update (OnScrollMessage (Grow Bottom)) = do
    model <- get

    unless (Seq.null (pages model)) $ do
        modify $ \m -> m { scrollTime = scrollKey (pages m) }
        io_ $ consoleLog "Catalog Scroll Message Grow Bottom"
        issue NextPage

    where
        scrollKey :: CatalogPages -> Maybe (UTCTime, Integer)
        scrollKey = fmap (\post -> (C.bump_time post, C.thread_id post)) . getLast

update (OnScrollMessage (Trim Top)) =
    modify $ \m -> m { pages = trimFirstPage (pages m) }

update (OnScrollMessage _) = io_ $ consoleLog "Catalog UNIMPLEMENTED Scroll Message"

update (OnErrorMessage msg) =
    io_ $ consoleError ("Catalog Component OnErrorMessage decode failure: " <> toMisoString msg)

update (NewPageInDom postId _domRef) =
    io_ $ consoleLog $ "PAGE CREATED " <> toMisoString (show postId)

-- | Safely gets the last element of a Seq
lastOf :: Seq a -> Maybe a
lastOf s = case viewr s of
    EmptyR -> Nothing
    _ :> x -> Just x

firstOf :: Seq a -> Maybe a
firstOf s = case viewl s of
    EmptyL -> Nothing
    x :< _ -> Just x

-- | Gets the last post from the last page
getLast :: CatalogPages -> Maybe C.CatalogPost
getLast ps = lastOf ps >>= lastOf


trimFirstPage :: Seq (f a) -> Seq (f a)
trimFirstPage ps =
    case viewl ps of
        EmptyL  -> ps
        _ :< xs -> xs

trimLastPage :: Seq (f a) -> Seq (f a)
trimLastPage ps =
    case viewr ps of
        EmptyR  -> ps
        xs :> _ -> xs
