{-# LANGUAGE OverloadedStrings #-}

module Common.FrontEnd.JSONSettings where

import Miso
    ( View
    , consoleLog
    )
import Miso.String (toMisoString)
import Miso.Html
    ( meta_
    )
import Miso.Html.Property
    ( name_
    , content_
    )
import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSettings = JSONSettings
    { postgrest_url :: String
    , jwt :: String
    , postgrest_fetch_count :: Int
    , media_root :: String
    , static_serve_path :: String
    , static_serve_url_root :: String
    } deriving (Show, Generic)

instance FromJSON JSONSettings

asHtml :: JSONSettings -> [ View model action ]
asHtml settings =
    [ meta "postgrest-url" (toMisoString $ postgrest_url settings)
    , meta "postgrest-fetch-count" (toMisoString $ postgrest_fetch_count settings)
    , meta "media-root" (toMisoString $ media_root settings)
    ]

    where
        meta name value = meta_ [ name_ name, content_ value ]

fromHtml :: JSM JSONSettings
fromHtml = do
    postgrestUrl <- getMetadata "postgrest-url" >>=
        return . maybe "http://localhost:3000" id
    consoleLog $ "postgrest-url " <> postgrestUrl

    postgrestFetchCount <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 fromMisoString

    mediaRoot <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    consoleLog $ "media_root: " <> mediaRoot

    return JSONSettings
        { postgrest_url = postgrestUrl
        , jwt = ""
        , postgrest_fetch_count = postgrestFetchCount
        , media_root = mediaRoot
        , static_serve_path = ""
        , static_serve_url_root = ""
        }


getMetadata :: MisoString -> JSM (Maybe MisoString)
getMetadata key = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "meta[name='" <> (fromMisoString key) <> "']"

    case mElem of
        Nothing -> return Nothing
        Just (Element el) ->
            (toMisoString <$>) <$> getAttribute el ("content" :: JSString)
