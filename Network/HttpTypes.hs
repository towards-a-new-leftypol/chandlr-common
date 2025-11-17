{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Network.HttpTypes where

import Miso (MisoString)
import GHC.Generics (Generic)
import Control.Concurrent.MVar (MVar)
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Aeson (Value, ToJSON, FromJSON)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult
    = Error MisoString
    | HttpResponse
        { status_code :: Int
        , status_text :: MisoString
        , body        :: Maybe Value
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


type HttpActionResult = (JSM (), MVar HttpResult) -- (abort, result)
