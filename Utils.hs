{-# LANGUAGE OverloadedStrings #-}

module Common.Utils where

import Data.Aeson (FromJSON, fromJSON, Result(..))
import Miso (Effect, consoleError, consoleLog, io_)
import Miso.String (toMisoString)

import qualified Common.Network.HttpTypes as Http

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
