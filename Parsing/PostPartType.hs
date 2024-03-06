module Common.Parsing.PostPartType where

import Miso.String (MisoString)
import Text.Parsec (ParseError)
import Data.Map (Map)

import Common.Network.PostType (Post)
import Common.Parsing.QuoteLinkParser (ParsedURL)

data PostPart
    = SimpleText MisoString
    | PostedUrl MisoString
    | Skip
    | Quote (Either ParseError ParsedURL)
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
    deriving (Show, Eq)


type Backlinks = Map Integer [ Post ]

