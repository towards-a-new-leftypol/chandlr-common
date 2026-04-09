{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Parsing.PostPartType where

import GHC.Generics
import Miso.String (MisoString)
import Data.Map (Map)
import Miso.JSON

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
    deriving (Show, Eq, Generic)

instance ToJSON PostPart where
    toJSON (SimpleText s)        = object [ "tag" .= String "SimpleText",        "contents" .= String s ]
    toJSON (PostedUrl s)         = object [ "tag" .= String "PostedUrl",         "contents" .= String s ]
    toJSON Skip                  = object [ "tag" .= String "Skip" ]
    toJSON (Quote e)             = object
        [ "tag"  .= String "Quote"
        , "contents" .= case e of
              Left err  -> object [ "tag" .= String "Left",  "value" .= toJSON err ]
              Right url -> object [ "tag" .= String "Right", "value" .= toJSON url ]
        ]
    toJSON (GreenText ps)        = object [ "tag" .= String "GreenText",         "contents" .= toJSON ps ]
    toJSON (OrangeText ps)       = object [ "tag" .= String "OrangeText",        "contents" .= toJSON ps ]
    toJSON (RedText ps)          = object [ "tag" .= String "RedText",           "contents" .= toJSON ps ]
    toJSON (Spoiler ps)          = object [ "tag" .= String "Spoiler",           "contents" .= toJSON ps ]
    toJSON (Bold ps)             = object [ "tag" .= String "Bold",              "contents" .= toJSON ps ]
    toJSON (Underlined ps)       = object [ "tag" .= String "Underlined",        "contents" .= toJSON ps ]
    toJSON (Italics ps)          = object [ "tag" .= String "Italics",           "contents" .= toJSON ps ]
    toJSON (Strikethrough ps)    = object [ "tag" .= String "Strikethrough",     "contents" .= toJSON ps ]
    toJSON (Code ps)             = object [ "tag" .= String "Code",              "contents" .= toJSON ps ]

instance FromJSON PostPart where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString
        case tag of
            "SimpleText"   -> SimpleText <$> m .: "contents"
            "PostedUrl"    -> PostedUrl  <$> m .: "contents"
            "Skip"         -> pure Skip
            "Quote"        -> do
                argsObj <- (m .: "contents") :: Parser Object
                qTag    <- argsObj .: "tag"
                case qTag :: MisoString of
                    "Left"  -> Quote . Left  <$> argsObj .: "value"
                    "Right" -> Quote . Right <$> argsObj .: "value"
                    _       -> fail "Expected 'Left' or 'Right' in Quote args"

            "GreenText"     -> GreenText      <$> m .: "contents"
            "OrangeText"    -> OrangeText     <$> m .: "contents"
            "RedText"       -> RedText        <$> m .: "contents"
            "Spoiler"       -> Spoiler        <$> m .: "contents"
            "Bold"          -> Bold           <$> m .: "contents"
            "Underlined"    -> Underlined     <$> m .: "contents"
            "Italics"       -> Italics        <$> m .: "contents"
            "Strikethrough" -> Strikethrough  <$> m .: "contents"
            "Code"          -> Code           <$> m .: "contents"
            _               -> fail "Unknown PostPart tag"

    parseJSON _ = fail "Expected Object for PostPart"


type Backlinks = Map Integer [ Post ]
