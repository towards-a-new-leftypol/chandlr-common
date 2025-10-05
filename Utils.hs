{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Utils where

import Data.Aeson
    ( FromJSON, fromJSON, Result(..), eitherDecodeStrict )
import Miso (Effect, consoleError, consoleLog, io_)
import Miso.String
    ( MisoString,
      toMisoString,
      MisoString,
      toMisoString,
      fromMisoString )
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Time.Clock (getCurrentTime)
import JSFFI.Saddle
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , textContent
    )
import qualified Data.ByteString.Base64 as B64
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import qualified Common.Network.HttpTypes as Http
import Common.FrontEnd.Types

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


getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e


getInitialDataPayload :: JSM InitialDataPayload
getInitialDataPayload = do
    maybeRawData <- getScriptContents "initial-data"

    let rawData = (B64.decode $ fromMisoString (fromMaybe "" maybeRawData)) >>= eitherDecodeStrict

    either
         ( \err -> do
             consoleLog $ "!!!! Could not parse initial data! Falling back to default values. Error: " <> toMisoString err
             t <- liftIO getCurrentTime
             return $ InitialDataPayload t Nil
         )
         ( \json -> do
            consoleLog "Successfully loaded base64 encoded JSON data from page"
            return json
        )
        rawData
