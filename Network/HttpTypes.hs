{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Network.HttpTypes where

import Miso (MisoString)
import Control.Concurrent.MVar (MVar)
import Miso.JSON hiding (Error)
import GHC.Float (int2Double)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult
    = Error MisoString
    | HttpResponse
        { status_code :: Int
        , status_text :: MisoString
        , body        :: Maybe Value
        }
    deriving (Show, Eq)

instance ToJSON HttpResult where
    toJSON (Error msg) = object
        [ "tag"     .= String "Error"
        , "message" .= String msg
        ]

    toJSON (HttpResponse {..}) = object
        [ "tag"         .= String "HttpResponse"
        , "status_code" .= Number (int2Double status_code)
        , "status_text" .= String status_text
        , "body"        .= toJSON body
        ]

instance FromJSON HttpResult where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString

        case tag of
            "Error"        -> Error <$> m .: "message"

            "HttpResponse" -> HttpResponse <$> m .: "status_code"
                                           <*> m .: "status_text"
                                           <*> m .: "body"

            _              -> fail "Unknown HttpResult tag"

    parseJSON _ = fail "Expected Object for HttpResult"

type HttpActionResult = (IO (), MVar HttpResult) -- (abort, result)
