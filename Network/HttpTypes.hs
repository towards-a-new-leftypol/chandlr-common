module Common.Network.HttpTypes where

import Control.Concurrent.MVar (MVar)
import Language.Javascript.JSaddle.Monad (JSM)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult a
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe a
        }
    deriving Eq

type HttpActionResult a = (JSM (), MVar (HttpResult a)) -- (abort, result)
