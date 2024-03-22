{-# LANGUAGE OverloadedStrings #-}

-- This is only common for server-side, not used by front-end
module Common.Network.HttpClient
( HttpError(..)
, get
, post
) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple hiding (httpLbs)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Client
    ( newManager
    , managerSetMaxHeaderLength
    , httpLbs
    , responseTimeoutNone
    )
import Network.HTTP.Client.Conduit (defaultManagerSettings)
import qualified Data.ByteString.Char8 as C8
import Control.Exception.Safe (tryAny, SomeException)

import qualified JSONSettings as T

data HttpError
    = HttpException SomeException
    | StatusCodeError Int LBS.ByteString
    deriving (Show)

get :: T.JSONSettings -> String -> IO (Either HttpError LBS.ByteString)
get settings path = do
    let requestUrl = T.postgrest_url settings ++ path
    initReq <- parseRequest requestUrl
    let req = setRequestHeader "Authorization" [C8.pack $ "Bearer " ++ T.jwt settings] initReq
    putStrLn $ "calling " ++ requestUrl

    let man_settings = managerSetMaxHeaderLength (16384 * 4) defaultManagerSettings
    manager <- newManager man_settings
    handleHttp (httpLbs req manager)


post
  :: T.JSONSettings
  -> String
  -> LBS.ByteString
  -> Bool
  -> IO (Either HttpError LBS.ByteString)
post settings path payload return_repr = do
    let requestUrl = T.postgrest_url settings ++ path
    req <- parseRequest requestUrl
    let initReq = setRequestResponseTimeout responseTimeoutNone req
    let request = setRequestMethod "POST"
            . setRequestHeader "Authorization" [ jwt_header ]
            . setRequestHeader "Content-Type" [ "application/json" ]
            . setRequestBodyLBS payload
            . prefer
            $ initReq

    putStrLn $ "posting to " ++ requestUrl
    -- putStrLn $ "Payload: " ++ (LC8.unpack payload)
    handleHttp (httpLBS request)

    where
      jwt_header = C8.pack $ "Bearer " ++ T.jwt settings
      prefer =
        if return_repr
        then setRequestHeader "Prefer" [ "return=representation" ]
        else id


handleHttp :: IO (Response LBS.ByteString) -> IO (Either HttpError LBS.ByteString)
handleHttp action = do
    result <- tryAny action
    case result of
        Right response -> 
            let responseBody = getResponseBody response
            in if 200 <= (statusCode $ getResponseStatus response) && (statusCode $ getResponseStatus response) < 300
               then return $ Right responseBody
               else return $ Left (StatusCodeError (statusCode $ getResponseStatus response) responseBody)
        Left e -> do
            putStrLn "Some nasty http exception must have occurred"
            return $ Left $ HttpException e


