{-# LANGUAGE OverloadedStrings #-}

module Common.Parsing.PostPartType where

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
    deriving (Show, Eq)

instance ToJSON PostPart where
    toJSON (SimpleText s)        = object [ "tag" .= String "SimpleText",        "args" .= String s ]
    toJSON (PostedUrl s)         = object [ "tag" .= String "PostedUrl",         "args" .= String s ]
    toJSON Skip                  = object [ "tag" .= String "Skip" ]
    toJSON (Quote e)             = object
        [ "tag"  .= String "Quote"
        , "args" .= case e of
              Left err  -> object [ "tag" .= String "Left",  "value" .= toJSON err ]
              Right url -> object [ "tag" .= String "Right", "value" .= toJSON url ]
        ]
    toJSON (GreenText ps)        = object [ "tag" .= String "GreenText",         "args" .= toJSON ps ]
    toJSON (OrangeText ps)       = object [ "tag" .= String "OrangeText",        "args" .= toJSON ps ]
    toJSON (RedText ps)          = object [ "tag" .= String "RedText",           "args" .= toJSON ps ]
    toJSON (Spoiler ps)          = object [ "tag" .= String "Spoiler",           "args" .= toJSON ps ]
    toJSON (Bold ps)             = object [ "tag" .= String "Bold",              "args" .= toJSON ps ]
    toJSON (Underlined ps)       = object [ "tag" .= String "Underlined",        "args" .= toJSON ps ]
    toJSON (Italics ps)          = object [ "tag" .= String "Italics",           "args" .= toJSON ps ]
    toJSON (Strikethrough ps)    = object [ "tag" .= String "Strikethrough",     "args" .= toJSON ps ]
    toJSON (Code ps)             = object [ "tag" .= String "Code",              "args" .= toJSON ps ]

instance FromJSON PostPart where
    parseJSON (Object m) = do
        tag <- (m .: "tag") :: Parser MisoString
        case tag of
            "SimpleText"   -> SimpleText <$> m .: "args"
            "PostedUrl"    -> PostedUrl  <$> m .: "args"
            "Skip"         -> pure Skip
            "Quote"        -> do
                argsObj <- (m .: "args") :: Parser Object
                qTag    <- argsObj .: "tag"
                case qTag :: MisoString of
                    "Left"  -> Quote . Left  <$> argsObj .: "value"
                    "Right" -> Quote . Right <$> argsObj .: "value"
                    _       -> fail "Expected 'Left' or 'Right' in Quote args"
            "GreenText"    -> GreenText <$> m .: "args"
            "OrangeText"   -> OrangeText <$> m .: "args"
            "RedText"      -> RedText <$> m .: "args"
            "Spoiler"      -> Spoiler <$> m .: "args"
            "Bold"         -> Bold <$> m .: "args"
            "Underlined"   -> Underlined <$> m .: "args"
            "Italics"      -> Italics <$> m .: "args"
            "Strikethrough" -> Strikethrough <$> m .: "args"
            "Code"         -> Code <$> m .: "args"
            _              -> fail "Unknown PostPart tag"
    parseJSON _ = fail "Expected Object for PostPart"


type Backlinks = Map Integer [ Post ]
