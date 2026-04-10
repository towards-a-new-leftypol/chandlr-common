{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

module Common.Utils where

import Prelude hiding (last)
import Miso
    ( Effect
    , consoleError
    , consoleLog
    , io_
    , View
    , URI
    , MisoString
    )
import Miso.JSON
import Miso.String (toMisoString, fromMisoString, last)
import Servant.Miso.Router (route)
import Data.Proxy (Proxy (..))
import Servant.API hiding (URI)
import Data.Either (fromRight)
import Data.Time.Clock
    ( UTCTime (..)
    , secondsToDiffTime
    )
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.List.NonEmpty (NonEmpty, toList, fromList)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

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


utcToIso :: UTCTime -> MisoString
utcToIso = toMisoString . iso8601Show

isoToUtc :: MisoString -> Parser UTCTime
isoToUtc t = do
    let str = fromMisoString t
        fmt = formatType t
    case parseTimeM True defaultTimeLocale fmt str of
        Just utc -> pure utc
        Nothing  -> fail $ "Invalid ISO8601 timestamp: " ++ str

    where
        formatType :: MisoString -> String
        formatType s
            | last s == 'Z' = "%Y-%m-%dT%H:%M:%S%QZ"
            | otherwise = "%Y-%m-%dT%H:%M:%S%Q%z"

instance ToJSON UTCTime where
    toJSON = String . utcToIso

instance FromJSON UTCTime where
    parseJSON (String x) = isoToUtc x
    parseJSON _ = fail "Expected String for UTCTime"


instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON . toList

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON (Array (x:xs)) = traverse parseJSON (fromList (x:xs))
    parseJSON _ = fail "Expected non empty Array for NonEmpty"
