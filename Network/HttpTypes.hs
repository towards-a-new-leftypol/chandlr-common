module Common.Network.HttpTypes where

import Control.Concurrent.MVar (MVar)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult a
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe a
        }

type HttpActionResult a = (IO (), MVar (HttpResult a)) -- (abort, result)
