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
    , MisoString
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

helperE
    :: (FromJSON a)
    => forall parent
    . Http.HttpResult
    -> (a -> Effect parent model action)
    -> (MisoString -> Effect parent model action)
    -> Effect parent model action
helperE (Http.Error e) _ onError = onError e
helperE (Http.HttpResponse status_code status_text (Just body)) continue onError = do
    io_ $ do
        consoleLog $ (toMisoString $ show $ status_code) <> " " <> (toMisoString $ status_text)
        -- consoleLog $ (toMisoString $ show $ body)

    let parsed = fromJSON body

    case parsed of
        Error msg -> onError (toMisoString msg) -- alert Error component here, maybe have toast pop up
        Success x -> continue x

helperE (Http.HttpResponse _ _ Nothing) _ _ = return ()

helper
    :: (FromJSON a)
    => forall parent
    . Http.HttpResult
    -> (a -> Effect parent model action)
    -> Effect parent model action
helper result onSuccess =
    helperE result onSuccess (io_ . consoleError)


data PageType = Catalog | Search (Maybe String) | Thread
    deriving Eq


pageTypeFromURI :: URI -> PageType
pageTypeFromURI = do
    -- default to Catalog in case of routing error.
    fromRight Catalog . routeResult

    where
        routeResult uri = route (Proxy :: Proxy (Route (View () ()))) handlers (const uri) undefined

        handlers = hLatest :<|> hThread :<|> hSearch

        hLatest :: m -> a -> PageType
        hLatest = const $ const Catalog

        hThread :: a -> a -> b -> m -> PageType
        hThread = const $ const $ const $ const Thread

        hSearch :: Maybe String -> m -> PageType
        hSearch = const . Search


fakeTime :: UTCTime
fakeTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
