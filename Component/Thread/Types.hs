{-# LANGUAGE OverloadedStrings #-}

module Common.Component.Thread.Types where

import Data.Time.Clock (UTCTime)
import Miso (MisoString, Topic, topic)
import Miso.JSON

import Common.Component.Thread.Model (PostWithBody)
import Common.Network.SiteType (Site)


data Action
    = OnMessage Message
    | OnMessageError MisoString
    | UpdatePostBodies UTCTime [ PostWithBody ]
    | Initialize
    | OnDeleteBtn PostWithBody
    deriving Eq


data Message
    = RenderSite MisoString Site
    | PostDeleted [ Integer ]
    deriving Eq

instance ToJSON Message where
    toJSON (RenderSite name site) = object
        [ "tag"  .= String "RenderSite"
        , "args" .= object [ "name" .= String name, "site" .= toJSON site ]
        ]
    toJSON (PostDeleted ids) = object
        [ "tag"  .= String "PostDeleted"
        , "args" .= Array (map (Number . fromIntegral) ids)
        ]

instance FromJSON Message where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString

        case tag of
            "RenderSite" -> do
                argsObj <- (m .: "args") :: Parser Object
                RenderSite <$> argsObj .: "name" <*> argsObj .: "site"

            "PostDeleted" -> PostDeleted <$> m .: "args"

            _             -> fail "Unknown Message tag"

    parseJSON _ = fail "Expected Object for Message"

threadTopic :: Topic Message
threadTopic = topic "thread"
