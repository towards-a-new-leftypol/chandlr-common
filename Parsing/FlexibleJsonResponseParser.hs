{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{-
 - The purpose of this module is to provide an alternative implementation of
 - FromJson for Sites -> Threads -> Boards -> Posts -> Attachments
 -
 - This quintuplet of objects sometimes has a few json representations
 - that are top-down, bottom-up or some other ordering because of postgrest.
 -}

module Common.Parsing.FlexibleJsonResponseParser
( parseSiteFromJson
)
where

import Data.Aeson
    ( Value (..)
    , Object
    , (.:)
    , (.:?)
    )
import Data.Aeson.Types (Parser)
import Data.Vector (toList)
import qualified Data.Aeson.KeyMap as Map
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as L

import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as B
import qualified Common.Network.ThreadType as T
import qualified Common.Network.PostType as P
import qualified Common.AttachmentType as A


data ObjectType = Site | Board | Thread | Post | Attachment


data Thing
    = TSite Site.Site
    | TBoard B.Board
    | TThread T.Thread
    | TPost P.Post
    | TAttachment A.Attachment


parseTopObject :: Object -> Parser Site.Site
parseTopObject o
    | objIsAttachment o = do
        a <- parseAttachment o
        restOfThings <- descend o
        assemble $ TAttachment a : restOfThings

    | objIsPost o = do
        a <- parsePost o
        restOfThings <- descend o
        assemble $ TPost a : restOfThings

    | objIsThread o = do
        a <- parseThread o
        restOfThings <- descend o
        assemble $ TThread a : restOfThings

    | objIsBoard o = do
        a <- parseBoard o
        restOfThings <- descend o
        assemble $ TBoard a : restOfThings

    | objIsSite o = do
        a <- parseSite o
        restOfThings <- descend o
        assemble $ TSite a : restOfThings

    | otherwise = fail "Encountered unexpected object"


assemble :: [ Thing ] -> Parser Site.Site
assemble = f . collapseThings
    where
        f (TSite s : _) = return s
        f _ = fail "Don't have any Sites"


-- Group by type and shove everything into everything else, heirarchially,
-- returns highest non-empty list of things in the heirarchy
collapseThings :: [ Thing ] -> [ Thing ]
collapseThings things
  | not (null sites)   = map TSite sitesWBoards
  | not (null boards)  = map TBoard boardsWThreads
  | not (null threads) = map TThread threadsWPosts
  | not (null posts)   = map TPost postsWAttachments
  | otherwise          = map TAttachment atts

  where
      sites   = [s | TSite s       <- things]
      boards  = [b | TBoard b      <- things]
      threads = [t | TThread t     <- things]
      posts   = [p | TPost p       <- things]
      atts    = [a | TAttachment a <- things]

      postsWAttachments
        | null atts = posts
        | otherwise = map (\p -> p { P.attachments = atts }) posts

      threadsWPosts
        | null posts = threads
        | otherwise = map (\t -> t { T.posts = L.fromList postsWAttachments }) threads

      boardsWThreads
        | null threads = boards
        | otherwise = map (\b -> b { B.threads = L.fromList threadsWPosts }) boards

      sitesWBoards
        | null boards = sites
        | otherwise = map (\s -> s { Site.boards = L.fromList boardsWThreads }) sites


descend :: Object -> Parser [ Thing ]
descend o =
    concat <$> mapM
        (\(k, t) -> do
            let nextval = fromJust $ Map.lookup k o
            parseNextObject t nextval
        )
        nxt
    where
        nxt = nextObjects o


parseNextObject :: ObjectType -> Value -> Parser [ Thing ]
parseNextObject Site (Object o) = do
    sites <- (: []) . TSite <$> parseSite o
    restOfThings <- descend o
    return (sites ++ restOfThings)

parseNextObject Board (Object o) = do
    boards <- (: []) . TBoard <$> parseBoard o
    restOfThings <- descend o
    return (boards ++ restOfThings)

parseNextObject Thread (Object o) = do
    threads <- (: []) . TThread <$> parseThread o
    restOfThings <- descend o
    return (threads ++ restOfThings)

parseNextObject Post (Object o) = do
    posts <- (: []) . TPost <$> parsePost o
    restOfThings <- descend o
    return (posts ++ restOfThings)

parseNextObject Attachment (Object o) = do
    attachments <- (: []) . TAttachment <$> parseAttachment o
    restOfThings <- descend o
    return (attachments ++ restOfThings)

parseNextObject t (Array arr) =
    mapM (parseNextObject t) (toList arr) >>= return . collapseThings . concat
parseNextObject _ (String _) = fail "Unexpected String JSON Value"
parseNextObject _ (Number _) = fail "Unexpected Number JSON Value"
parseNextObject _ (Bool _) = fail "Unexpected Bool JSON Value"
parseNextObject _ Null = fail "Unexpected Null JSON Value"


nextObjects :: Object -> [ (Map.Key, ObjectType) ]
nextObjects o = filter (\(k, _) -> Map.member k o) objectTypeKeys


objectTypeKeys :: [ (Map.Key, ObjectType) ]
objectTypeKeys =
    [ ("sites", Site)
    , ("boards", Board)
    , ("threads", Thread)
    , ("posts", Post)
    , ("attachments", Attachment)
    ]


parseSiteFromJson :: Value -> Parser [ Site.Site ]
parseSiteFromJson (Object obj) = (: []) <$> parseTopObject obj
parseSiteFromJson (Array arr) =
    mapM parseSiteFromJson (toList arr) >>= return . concat

parseSiteFromJson _ = fail "Expected an array or an object at the top level"


objIsAttachment :: Object -> Bool
objIsAttachment o =
    Map.member "attachment_id" o
    &&
    Map.member "post_id" o

objIsSite :: Object -> Bool
objIsSite o =
    Map.member "site_id" o
    &&
    Map.member "url" o

objIsBoard :: Object -> Bool
objIsBoard o =
    Map.member "board_id" o
    &&
    Map.member "site_id" o

objIsThread :: Object -> Bool
objIsThread o =
    Map.member "thread_id" o
    &&
    Map.member "board_id" o

-- here we have to be careful if we ever denormalize anything into post
objIsPost :: Object -> Bool
objIsPost o =
    Map.member "post_id" o
    &&
    Map.member "thread_id" o


parseAttachment :: Object -> Parser A.Attachment
parseAttachment o = do
    attachment_id_     <- o .:  "attachment_id"
    mimetype_          <- o .:  "mimetype"
    creation_time_     <- o .:  "creation_time"
    sha256_hash_       <- o .:  "sha256_hash"
    phash_             <- o .:? "phash"
    illegal_           <- o .:  "illegal"
    post_id_           <- o .:  "post_id"
    resolution_        <- o .:? "resolution"
    file_extension_    <- o .:? "file_extension"
    thumb_extension_   <- o .:? "thumb_extension"
    original_filename_ <- o .:? "original_filename"
    board_filename_    <- o .:  "board_filename"
    spoiler_           <- o .:  "spoiler"
    file_size_bytes_   <- o .:  "file_size_bytes"
    attachment_idx_    <- o .:  "attachment_idx"

    pure $ A.Attachment
        { A.attachment_id     = attachment_id_
        , A.mimetype          = mimetype_
        , A.creation_time     = creation_time_
        , A.sha256_hash       = sha256_hash_
        , A.phash             = phash_
        , A.illegal           = illegal_
        , A.post_id           = post_id_
        , A.resolution        = resolution_
        , A.file_extension    = file_extension_
        , A.thumb_extension   = thumb_extension_
        , A.original_filename = original_filename_
        , A.board_filename    = board_filename_
        , A.spoiler           = spoiler_
        , A.file_size_bytes   = file_size_bytes_
        , A.attachment_idx    = attachment_idx_
        }


parseSite :: Object -> Parser Site.Site
parseSite o = do
   site_id_ <- o .: "site_id"
   name_    <- o .: "name"
   url_     <- o .: "url"

   pure $ Site.Site
        { Site.site_id = site_id_
        , Site.name    = name_
        , Site.url     = url_
        , Site.boards  = undefined
        }


parseBoard :: Object -> Parser B.Board
parseBoard o = do
    board_id_  <- o .:  "board_id"
    name_      <- o .:? "name"
    pathpart_  <- o .:  "pathpart"
    site_id_   <- o .:  "site_id"

    pure $ B.Board
        { B.board_id = board_id_
        , B.name     = name_
        , B.pathpart = pathpart_
        , B.site_id  = site_id_
        , B.threads  = undefined
        }


parseThread :: Object -> Parser T.Thread
parseThread o = do
    thread_id_       <- o .: "thread_id"
    board_thread_id_ <- o .: "board_thread_id"
    creation_time_   <- o .: "creation_time"
    board_id_        <- o .: "board_id"

    pure $ T.Thread
        { T.thread_id       = thread_id_
        , T.board_thread_id = board_thread_id_
        , T.creation_time   = creation_time_
        , T.board_id        = board_id_
        , T.posts           = undefined
        }


parsePost :: Object -> Parser P.Post
parsePost o = do
    post_id_           <- o .:  "post_id"
    board_post_id_     <- o .:  "board_post_id"
    creation_time_     <- o .:  "creation_time"
    body_              <- o .:? "body"
    subject_           <- o .:? "subject"
    local_idx_         <- o .:  "local_idx"
    name_              <- o .:? "name"
    email_             <- o .:? "email"
    body_search_index_ <- o .:  "body_search_index"
    thread_id_         <- o .:  "thread_id"
    embed_             <- o .:? "embed"

    pure $ P.Post
        { P.post_id           = post_id_
        , P.board_post_id     = board_post_id_
        , P.creation_time     = creation_time_
        , P.body              = body_
        , P.subject           = subject_
        , P.local_idx         = local_idx_
        , P.name              = name_
        , P.email             = email_
        , P.body_search_index = body_search_index_
        , P.thread_id         = thread_id_
        , P.embed             = embed_
        , P.attachments       = undefined
        }
