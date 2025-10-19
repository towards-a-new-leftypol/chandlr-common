{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Utils where

import Data.Aeson
    ( FromJSON
    , fromJSON
    , Result(..)
    )
import Miso
    ( Effect
    , consoleError
    , consoleLog
    , io_
    , View
    , URI
    )
import Miso.String (toMisoString)
import Servant.Miso.Router (route)
import Data.Proxy (Proxy (..))
import Servant.API hiding (URI)
import Data.Either (fromRight)
import Data.Time.Clock
    ( UTCTime (..)
    , secondsToDiffTime
    )
import Data.Time.Calendar (fromGregorian)

import qualified Common.Network.HttpTypes as Http
import Common.FrontEnd.Routes (Route)

helper
    :: (FromJSON a)
    => forall parent
    . Http.HttpResult
    -> (a -> Effect parent model action)
    -> Effect parent model action
helper Http.Error _ = io_ $ consoleError "Http Error"
helper (Http.HttpResponse status_code status_text (Just body)) continue = do
    io_ $ do
        consoleLog $ (toMisoString $ show $ status_code) <> " " <> (toMisoString $ status_text)
        -- consoleLog $ (toMisoString $ show $ body)

    let parsed = fromJSON body

    case parsed of
        Error msg -> io_ $ consoleError (toMisoString msg) -- alert Error component here, maybe have toast pop up
        Success x -> continue x

helper _ _ = return () -- No body, nothing to parse


data PageType = Catalog | Search (Maybe String) | Thread
    deriving Eq


pageTypeFromURI :: URI -> PageType
pageTypeFromURI = do
    -- default to Catalog in case of routing error.
    fromRight Catalog . routeResult

    where
        routeResult uri = route (Proxy :: Proxy (Route (View () ()))) handlers (const uri) undefined

        handlers = hLatest :<|> hThread :<|> hSearch

        hLatest :: m -> PageType
        hLatest = const Catalog

        hThread :: a -> a -> b -> m -> PageType
        hThread = const $ const $ const $ const Thread

        hSearch :: Maybe String -> m -> PageType
        hSearch = const . Search


fakeTime :: UTCTime
fakeTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
