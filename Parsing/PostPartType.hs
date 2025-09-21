{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Parsing.PostPartType where

import Miso.String (MisoString)
import Data.Map (Map)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Common.Network.PostType (Post)
import Common.Parsing.QuoteLinkParser (ParsedURL, UrlParseError)

data PostPart
    = SimpleText MisoString
    | PostedUrl MisoString
    | Skip
    | Quote (Either UrlParseError ParsedURL)
        -- Quotes don't seem to be able to be spoilered
        -- board links (which appear as quotes but start with >>>) break the tag
    | GreenText     [ PostPart ]
    | OrangeText    [ PostPart ]
    | RedText       [ PostPart ]
    | Spoiler       [ PostPart ]
    -- you can't seem to spoiler greentext
    | Bold          [ PostPart ]
    | Underlined    [ PostPart ]
    | Italics       [ PostPart ]
    | Strikethrough [ PostPart ]
    | Code          [ PostPart ]
    deriving (Show, Eq, Generic, FromJSON, ToJSON)


type Backlinks = Map Integer [ Post ]

