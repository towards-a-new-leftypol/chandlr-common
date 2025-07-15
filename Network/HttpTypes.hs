{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.HttpTypes where

import GHC.Generics (Generic)
import Control.Concurrent.MVar (MVar)
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Aeson (Value, ToJSON, FromJSON)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe Value
        }
    deriving (Eq, Generic, ToJSON, FromJSON)

-- TODO: maybe HttpResult a shouldn't have the 'a' param, but have the body be a Value (from aeson)
-- this would mean that http is no longer decoding anything, but Client users do.
--      - then there needs to be a generic way to decode the message into the
--      right value type, as well as perhaps some global decoding error handling (message an Error component?)

type HttpActionResult = (JSM (), MVar HttpResult) -- (abort, result)
